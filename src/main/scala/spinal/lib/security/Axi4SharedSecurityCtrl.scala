package spinal.lib.security

import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4Arw, Axi4Config, Axi4Shared}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.memory.sdram.{Axi4SharedSdramCtrl, SdramLayout}
import spinal.lib.{Counter, CounterUpDown, Fragment, Stream, StreamFifo, master, slave}

/**
  * Created by Jiangyi on 2019-07-14.
  */
object Axi4SharedSecurityCtrl {
  def getAxiConfig(dataWidth: Int, addressWidth: Int, idWidth: Int): Axi4Config = {
    Axi4Config(
      addressWidth = addressWidth,
      dataWidth = dataWidth,
      idWidth = idWidth,
      useLock = false,
      useRegion = false,
      useCache = false,
      useProt = false,
      useQos = false
    )
  }
}


case class Axi4SharedSecurityCtrl(axiDataWidth: Int, axiAddrWidth: Int, axiIdWidth: Int, layout : SdramLayout) extends Component {
  val axiConfig = Axi4SharedSdramCtrl.getAxiConfig(axiDataWidth, axiIdWidth, layout)
  val writeToRam = Reg(Bool())
  val error = Bool()
  final val treeAry = 4

  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    val sdramAxi = master(Axi4Shared(axiConfig))
  }

  // Data = 8 bytes, tag = 4 bytes, nonce = 2 bytes
  val dataInFifo = StreamFifo(dataType = Fragment(Bits(axiDataWidth bits)), depth = 14)
  val dataOutFifo = StreamFifo(dataType = Fragment(Bits(axiDataWidth bits)), depth = 14)

  val dataSetAsideFifo = StreamFifo(dataType = Fragment(Bits(axiDataWidth bits)), depth = 8)

  // Tree address registers
  val currentNodeFirstSiblingStartAddrOffsetReg = Reg(UInt(axiConfig.addressWidth bits))
  val currentAddrOffsetReg = Reg(UInt(axiConfig.addressWidth bits))
  val currentLevelStartAddrReg = Reg(UInt(axiConfig.addressWidth bits))

  val localTreeCounter = Counter(0 to 4) // 4 leaf nodes + 1 spare state for reading parent node
  val pendingWordsCounter = Counter(0 to 7)
  val caesarCtrl = CAESARCtrl(axiConfig)

//  val sharedCmdReg = RegNextWhen(io.axi.sharedCmd, io.axi.sharedCmd.fire)

  val sdramWrEnReg = RegInit(False)
  val writeRspValidReg = RegInit(False)
  val sdramAxiSharedCmdValidReg = RegInit(False)

  val nonceVecReg = Vec(Bits(axiConfig.dataWidth bits), 4).setAsReg()

  val axiSharedCmdReg = Reg(Stream(Axi4Arw(axiConfig)))
  val dataReg = Reg(Bits(axiDataWidth bits))
  val strbReg = Reg(Bits(axiConfig.bytePerWord bits))
  val busyReg = RegInit(False)


  def bytePosition(addr: UInt) : UInt = addr(4 downto 2)

  def combineNewDataWithOriginal(originalData: Bits, newData: Bits, strbMask: Bits) : Bits = {
    assert(originalData.getWidth == newData.getWidth)
    assert(newData.getWidth == 32)
    val outData = newData

    when (strbMask(3)) {
      outData(31 downto 24) := newData(31 downto 24)
    }

    when (strbMask(2)) {
      outData(23 downto 16) := newData(23 downto 16)
    }

    when (strbMask(1)) {
      outData(15 downto 8) := newData(15 downto 8)
    }

    when (strbMask(0)) {
      outData(7 downto 0) := newData(7 downto 0)
    }

    outData
  }

  def getSdramDataAddress(originalAddr: UInt): UInt = {
    originalAddr(axiConfig.addressWidth - 1 downto 5) @@ U"00000"
  }

  // Assuming 4-ary for now
  def getTreeLeafNodeTagAddress(originalAddr: UInt): UInt = {
    0x4000000 + (originalAddr(axiConfig.addressWidth - 1 downto 5).resize(axiConfig.addressWidth) * 12)
  }

  def updateToNextParentNodeAddrReg(): Unit = {
    currentLevelStartAddrReg := getNextLevelStartAddr()
    currentAddrOffsetReg := getParentNodeAddrOffset()
    currentNodeFirstSiblingStartAddrOffsetReg := getFirstSiblingStartAddr(getParentNodeAddrOffset())
  }

  def getCurrentTagNodeAddr(): UInt = {
    currentLevelStartAddrReg + currentAddrOffsetReg
  }

  def getCurrentTagNodeFirstSiblingAddr() : UInt = {
    currentLevelStartAddrReg + currentNodeFirstSiblingStartAddrOffsetReg
  }

  def getFirstSiblingStartAddr(addr: UInt): UInt = (addr / 0x300) * 0x300

  def getNextLevelStartAddr(): UInt = currentLevelStartAddrReg + (currentLevelStartAddrReg / 4)

  def getParentNodeAddrOffset() : UInt = currentAddrOffsetReg / 0x300 * 0xC0

  def getParentNodeAddr(): UInt = getNextLevelStartAddr() + getParentNodeAddrOffset()

  def isRootOfTree(): Bool = getCurrentTagNodeAddr() === 0x7FFFFFE8

  dataInFifo.io.push.valid := False
  dataInFifo.io.push.payload.assignDontCare()

  caesarCtrl.io.out_stream <> dataOutFifo.io.push
  caesarCtrl.io.in_stream <> dataInFifo.io.pop

  dataOutFifo.io.pop.ready := False

  dataSetAsideFifo.io.push.valid := False

  when (busyReg || io.axi.sharedCmd.addr < 0x4000000) {
    dataOutFifo.io.pop.ready := io.sdramAxi.writeData.ready


    io.sdramAxi.writeData.valid := dataOutFifo.io.pop.valid && sdramWrEnReg
    io.sdramAxi.writeData.data := dataOutFifo.io.pop.payload.fragment
    io.sdramAxi.writeData.strb := "1111"
    io.sdramAxi.writeData.last := dataOutFifo.io.pop.payload.last

    io.sdramAxi.sharedCmd.size := io.axi.sharedCmd.size
    //    when (io.sdramAxi.sharedCmd.write) {
    //      io.sdramAxi.sharedCmd.len  := 0
    //      io.sdramAxi.sharedCmd.addr := axiSharedCmdReg.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
    //
    //    } otherwise {
    when(axiSharedCmdReg.write) {
      io.sdramAxi.sharedCmd.len := 7
    } otherwise {
      io.sdramAxi.sharedCmd.len := axiSharedCmdReg.len
    }
    //      io.sdramAxi.sharedCmd.id := 8
    //    }
    io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
    io.sdramAxi.sharedCmd.id := axiSharedCmdReg.id
    io.sdramAxi.sharedCmd.burst := axiSharedCmdReg.burst
    io.sdramAxi.sharedCmd.write := sdramWrEnReg
    //    io.sdramAxi.sharedCmd.addr := io.axi.sharedCmd.addr
    io.sdramAxi.sharedCmd.valid := sdramAxiSharedCmdValidReg
    io.sdramAxi.writeRsp.ready := True


    io.axi.sharedCmd.ready := io.axi.sharedCmd.valid && !busyReg
    io.axi.writeData.ready := io.axi.writeData.valid && !busyReg
    io.axi.writeRsp.valid := writeRspValidReg
    io.axi.writeRsp.resp := io.sdramAxi.writeRsp.resp
    io.axi.writeRsp.payload.id := io.sdramAxi.writeRsp.payload.id

    io.axi.readRsp.valid := False
    io.axi.readRsp.data := io.sdramAxi.readRsp.data
    io.axi.readRsp.last := io.sdramAxi.readRsp.last
    io.axi.readRsp.id := io.sdramAxi.readRsp.id
    io.axi.readRsp.resp := io.sdramAxi.readRsp.resp

    io.sdramAxi.readRsp.ready := dataInFifo.io.push.ready

    val fsm = new StateMachine {
      val idleState: State = new State with EntryPoint {
        onEntry {
          pendingWordsCounter := 0
          sdramWrEnReg := False
        }

        whenIsActive {
          when(io.axi.sharedCmd.fire) {
            busyReg := True
            axiSharedCmdReg := io.axi.sharedCmd

            currentLevelStartAddrReg := 0x4000000
            currentAddrOffsetReg := io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5).resize(axiConfig.addressWidth) * 0xC
            currentNodeFirstSiblingStartAddrOffsetReg := getFirstSiblingStartAddr(io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5).resize(axiConfig.addressWidth) * 0xC)

            // For the read case, go directly to read state
            when(!io.axi.sharedCmd.write) {
              sdramAxiSharedCmdValidReg := True
              goto(readDataFromSdramState)
            }
          }

          // These two blocks are for the write case
          when(io.axi.writeData.fire) {
            writeRspValidReg := True
            dataReg := io.axi.writeData.data
            strbReg := io.axi.writeData.strb
            io.axi.writeRsp.isOKAY()
          }

          when(io.axi.writeRsp.fire) {
            writeRspValidReg := False
            sdramAxiSharedCmdValidReg := True
            goto(readDataFromSdramState)
          }
        }
      }

      val readDataFromSdramState: State = new State {
        val isReadingTag = RegInit(False)
        //    onEntry(pendingWordsCounter := 0)
        whenIsActive {
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          when(isReadingTag) {
            io.sdramAxi.sharedCmd.addr := getCurrentTagNodeAddr()
            io.sdramAxi.sharedCmd.len := 5 // 6 * 32 == 192 bits, which is the leaf node size
          } otherwise {
            io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
            io.sdramAxi.sharedCmd.len := 7 // 8 * 32 == 256 bits, AKA length of a cache line
          }

          caesarCtrl.io.mode := caesarCtrl.encMode
          io.sdramAxi.writeData.valid := False
          //          val axiSharedCmd = cloneOf(io.axi.sharedCmd)
          //
          //          axiSharedCmd.addr := io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
          //          axiSharedCmd.valid := True
          //          axiSharedCmd.write := False
          //
          //          io.sdramAxi.sharedCmd <> axiSharedCmd

          dataInFifo.io.push.payload.last := io.sdramAxi.readRsp.last && isReadingTag
          dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid
//          when(axiSharedCmdReg.write && pendingWordsCounter.value === bytePosition(axiSharedCmdReg.addr)) {
//            dataInFifo.io.push.payload.fragment := combineNewDataWithOriginal(io.sdramAxi.readRsp.data, dataReg, strbReg)
//          } otherwise {
          dataInFifo.io.push.payload.fragment := io.sdramAxi.readRsp.data
//          }
          dataOutFifo.io.pop >> dataSetAsideFifo.io.push

          when(dataInFifo.io.push.ready && io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.last) {
            isReadingTag := True
            sdramAxiSharedCmdValidReg := True
            when(isReadingTag) {
              goto(verifyTagFromSdramState)
            }
          }
        }
      }

      val verifyTagFromSdramState: State = new State {
        val readComplete = RegInit(False)
        io.sdramAxi.writeData.valid := False

        when (localTreeCounter.value < 4) {
          io.sdramAxi.sharedCmd.addr := getCurrentTagNodeFirstSiblingAddr() + (localTreeCounter << 2) + 16
          io.sdramAxi.sharedCmd.len := 1 // Burst of 2, since 64 bit nonces
        } otherwise {
          io.sdramAxi.sharedCmd.addr := getParentNodeAddr()
          io.sdramAxi.sharedCmd.len := 3 // Burst of 4, since 128 bit tag
        }

        when(io.sdramAxi.sharedCmd.fire) {
          sdramAxiSharedCmdValidReg := False
        }

        dataInFifo.io.push.payload.last := io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.last && localTreeCounter.willOverflowIfInc

        dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid
        dataInFifo.io.push.payload.fragment := io.sdramAxi.readRsp.data

        when(localTreeCounter.value < 4) {
          nonceVecReg(localTreeCounter.value) := io.sdramAxi.readRsp.data
        }

        when(io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.last) {
          when(localTreeCounter.willOverflowIfInc) {
            readComplete := True
          } otherwise {
            localTreeCounter.increment()
            sdramAxiSharedCmdValidReg := True
          }
        }

        caesarCtrl.io.error.ready := readComplete

        when (caesarCtrl.io.error.fire && !caesarCtrl.io.error.payload) {
          when(axiSharedCmdReg.write) {
            caesarCtrl.io.in_stream <> dataSetAsideFifo.io.pop
            caesarCtrl.io.mode := caesarCtrl.encMode
            io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
            sdramWrEnReg := True
            sdramAxiSharedCmdValidReg := True
            localTreeCounter := 0
            goto(writeToSdramState)
          } elsewhen(isRootOfTree()) {
            busyReg := False
            goto(idleState)
          } otherwise {
              localTreeCounter := 0
              updateToNextParentNodeAddrReg()
          }
        }
      }

      val writeToSdramState: State = new State {
        whenIsActive {
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          //          otherwise {
          //            io.sdramAxi.writeData.strb := "1111"
          //          }

          when(io.sdramAxi.writeRsp.fire) {
            //            pendingWordsCounter.increment()
            //            sdramAxiSharedCmdValidReg := True
            //            when(pendingWordsCounter.willOverflowIfInc) {
            when(isRootOfTree()) {
              busyReg := False
              goto(idleState)
            } otherwise {
              goto(updateTreeState)
            }
            //            }
          }
        }

        onExit {
          sdramWrEnReg := False
          sdramAxiSharedCmdValidReg := False
        }
      }

      val updateTreeState: State = new State {

      }

      // State for returning data to dcache
      val returnDataState: State = new State {
        whenIsActive {
          io.axi.readRsp.valid := dataOutFifo.io.pop.valid
          io.axi.readRsp.data := dataOutFifo.io.pop.payload.fragment
          io.axi.readRsp.last := dataOutFifo.io.pop.payload.last
          io.axi.readRsp.id := axiSharedCmdReg.id
          io.axi.readRsp.setOKAY()

          dataOutFifo.io.pop.ready := io.axi.readRsp.ready

          when(io.axi.readRsp.fire && io.axi.readRsp.last) {
            goto(idleState)
          }
        }
        onExit {
          sdramWrEnReg := False
          sdramAxiSharedCmdValidReg := False
          busyReg := False
        }
      }
    }
  } otherwise {
    // Accessing tree section; bypass and let it through directly
    io.axi <> io.sdramAxi
  }

}