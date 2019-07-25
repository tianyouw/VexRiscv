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
  val dataInFifo = StreamFifo(dataType = CAESARCtrlInData(axiConfig), depth = 14)
  val dataOutFifo = StreamFifo(dataType = CAESARCtrlOutData(axiConfig), depth = 14)

  val dataSetAsideFifo = StreamFifo(dataType = Fragment(Bits(axiDataWidth bits)), depth = 8)
  val nextNonceTagBlockFifo = StreamFifo(dataType = Fragment(Bits(axiConfig.dataWidth bits)), depth = 6)

  // Tree address registers
  val currentNodeFirstSiblingStartAddrOffsetReg = Reg(UInt(axiConfig.addressWidth bits))
  val currentAddrOffsetReg = Reg(UInt(axiConfig.addressWidth bits))
  val currentLevelStartAddrReg = Reg(UInt(axiConfig.addressWidth bits))

  val nonceVecReg = Vec(Reg(Bits(axiConfig.dataWidth bits)), 8)

  val decryptVerifyCounter = Counter(0 until 14)
  val pendingWordsCounter = Counter(0 until 8)

  val caesarCtrl = CAESARCtrl(axiConfig)

//  val sharedCmdReg = RegNextWhen(io.axi.sharedCmd, io.axi.sharedCmd.fire)

  val sdramWrEnReg = RegInit(False)
  val writeRspValidReg = RegInit(False)
  val sdramAxiSharedCmdValidReg = RegInit(False)


  val axiSharedCmdReg = Reg(Stream(Axi4Arw(axiConfig)))
  val dataReg = Reg(Bits(axiDataWidth bits))
  val strbReg = Reg(Bits(axiConfig.bytePerWord bits))
  val busyReg = RegInit(False)
  val caesarInputCmdValidReg = RegInit(False)




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

  def getFirstSiblingStartAddr(addr: UInt): UInt = (addr / 0x60) * 0x60

  def getNextLevelStartAddr(): UInt = currentLevelStartAddrReg + (currentLevelStartAddrReg / 4)

  def getParentNodeAddrOffset() : UInt = currentAddrOffsetReg / (0x60 * 0x18)

  def getParentNodeAddr(): UInt = getNextLevelStartAddr() + getParentNodeAddrOffset()

  def isRootOfTree(): Bool = getCurrentTagNodeAddr() === 0x7FFFFFE8

  def getSiblingIndex(): UInt = {
    val blockNum = currentAddrOffsetReg / 24
    blockNum - ((blockNum - 1) & ~0x03) - 1
  }

  dataInFifo.io.push.valid := False
  dataInFifo.io.push.payload.assignDontCare()

  caesarCtrl.io.out_stream <> dataOutFifo.io.push
  caesarCtrl.io.in_data_stream <> dataInFifo.io.pop

  dataOutFifo.io.pop.ready := False

  dataSetAsideFifo.io.push.valid := False

  when (busyReg || io.axi.sharedCmd.addr < 0x4000000) {
    dataOutFifo.io.pop.ready := io.sdramAxi.writeData.ready


    io.sdramAxi.writeData.valid := dataOutFifo.io.pop.valid && sdramWrEnReg
    io.sdramAxi.writeData.data := dataOutFifo.io.pop.payload.data.fragment
    io.sdramAxi.writeData.strb := "1111"
    io.sdramAxi.writeData.last := dataOutFifo.io.pop.payload.data.last

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

    caesarCtrl.io.in_cmd_stream.valid := caesarInputCmdValidReg

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
              caesarInputCmdValidReg := True
              sdramAxiSharedCmdValidReg := True
              goto(verifyTagFromSdramState)
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
            caesarInputCmdValidReg := True
            goto(verifyTagFromSdramState)
          }
        }
      }

      val verifyTagFromSdramState: State = new State {
        val readComplete = RegInit(False)
        val writeDataComplete = RegInit(False)
        io.sdramAxi.writeData.valid := False
        caesarCtrl.io.in_cmd_stream.mode := caesarCtrl.macVerMode
        caesarCtrl.io.out_stream.ready := readComplete

        when (decryptVerifyCounter.value < 8) {
          io.sdramAxi.sharedCmd.addr := getCurrentTagNodeFirstSiblingAddr() + (decryptVerifyCounter << 2)
          io.sdramAxi.sharedCmd.len := 1 // Burst of 2, since 64 bit nonces
        } otherwise {
          io.sdramAxi.sharedCmd.addr := getParentNodeAddr()
          io.sdramAxi.sharedCmd.len := 5 // Burst of 6, since 128 bit tag + 64 bit nonce
        }

        when(io.sdramAxi.sharedCmd.fire) {
          sdramAxiSharedCmdValidReg := False
        }

        when(caesarCtrl.io.in_cmd_stream.fire) {
          caesarInputCmdValidReg := False
        }

        dataInFifo.io.push.payload.data.last := io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.last && decryptVerifyCounter.willOverflowIfInc

        dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid
        dataInFifo.io.push.payload.data.fragment := io.sdramAxi.readRsp.data

        when(io.sdramAxi.readRsp.fire) {
          when(decryptVerifyCounter.value < 8) {
            nonceVecReg(decryptVerifyCounter.value) := io.sdramAxi.readRsp.data
          }

          when(io.sdramAxi.readRsp.last) {
            when(decryptVerifyCounter.willOverflowIfInc) {
              readComplete := True
            } otherwise {
              sdramAxiSharedCmdValidReg := True
              decryptVerifyCounter.increment()
            }
          } otherwise {
            decryptVerifyCounter.increment()
          }
        }

        when (caesarCtrl.io.out_stream.fire && !caesarCtrl.io.out_stream.error) {
          when(axiSharedCmdReg.write) {
            sdramAxiSharedCmdValidReg := True
            decryptVerifyCounter := 0
            when(!writeDataComplete) {
              goto(decryptDataState)
            } otherwise {
              when(isRootOfTree()) {
                busyReg := False
                goto(idleState)
              } otherwise {
                sdramAxiSharedCmdValidReg := True
                sdramWrEnReg := True
                goto(writeNewTagState)
              }
            }
          } otherwise {
            when (isRootOfTree()) {
              goto(decryptDataState)
            } otherwise {
              decryptVerifyCounter := 0
              updateToNextParentNodeAddrReg()
            }
          }
        }

        onExit {
          decryptVerifyCounter := 0
        }
      }

      val decryptDataState: State = new State {
        val isReadingTagReg = RegInit(True)
        //    onEntry(pendingWordsCounter := 0)
        whenIsActive {
          caesarCtrl.io.in_cmd_stream.mode := caesarCtrl.decMode

          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          when(isReadingTagReg) {
            io.sdramAxi.sharedCmd.addr := getCurrentTagNodeAddr()
            io.sdramAxi.sharedCmd.len := 5 // 6 * 32 == 192 bits, which is the leaf node size
          } otherwise {
            io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
            io.sdramAxi.sharedCmd.len := 7 // 8 * 32 == 256 bits, AKA length of a cache line
          }

          caesarCtrl.io.in_cmd_stream.mode := caesarCtrl.encMode
          io.sdramAxi.writeData.valid := False
          //          val axiSharedCmd = cloneOf(io.axi.sharedCmd)
          //
          //          axiSharedCmd.addr := io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
          //          axiSharedCmd.valid := True
          //          axiSharedCmd.write := False
          //
          //          io.sdramAxi.sharedCmd <> axiSharedCmd

          dataInFifo.io.push.payload.data.last := io.sdramAxi.readRsp.last && isReadingTagReg
          dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid
          //          when(axiSharedCmdReg.write && pendingWordsCounter.value === bytePosition(axiSharedCmdReg.addr)) {
          //            dataInFifo.io.push.payload.fragment := combineNewDataWithOriginal(io.sdramAxi.readRsp.data, dataReg, strbReg)
          //          } otherwise {
          dataInFifo.io.push.payload.data.fragment := io.sdramAxi.readRsp.data
          //          }

          dataSetAsideFifo.io.push.fragment := dataOutFifo.io.pop.data.fragment
          dataSetAsideFifo.io.push.last := dataOutFifo.io.pop.data.last
          dataSetAsideFifo.io.push.valid := dataOutFifo.io.pop.valid
          dataOutFifo.io.pop.ready := dataSetAsideFifo.io.push.ready

          when(dataInFifo.io.push.ready && io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.last) {
            isReadingTagReg := False
            when(!isReadingTagReg) {
              when(axiSharedCmdReg.write) {
                caesarInputCmdValidReg := True
                goto(encryptDataState)
              } otherwise {
                sdramWrEnReg := False
                sdramAxiSharedCmdValidReg := False
                goto(returnDataState)
              }
            }
          }
        }
      }

      val encryptDataState: State = new State {
        whenIsActive {
          caesarCtrl.io.in_cmd_stream.mode := caesarCtrl.encMode

          dataInFifo.io.push.valid := dataSetAsideFifo.io.pop.valid
          dataSetAsideFifo.io.pop.ready := dataInFifo.io.push.ready


          // Replace byte with what we want to write
          when (pendingWordsCounter.value === bytePosition(axiSharedCmdReg.addr)) {
            dataInFifo.io.push.data.fragment := combineNewDataWithOriginal(io.sdramAxi.readRsp.data, dataReg, strbReg)
            dataInFifo.io.push.data.last := dataSetAsideFifo.io.pop.last
          } otherwise {
            dataInFifo.io.push.data := dataSetAsideFifo.io.pop.payload
          }

          when (dataInFifo.io.push.fire) {
            pendingWordsCounter.increment()

            when(pendingWordsCounter.willOverflowIfInc) {
              sdramAxiSharedCmdValidReg := True
              sdramWrEnReg := True
              goto(writeDataState)
            }
          }
        }

        onExit {
          pendingWordsCounter := 0
        }
      }

      val writeDataState: State = new State {
        val doneWritingData = RegInit(False)
        whenIsActive {
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          when(caesarCtrl.io.in_cmd_stream.fire) {
            caesarInputCmdValidReg := False
          }

          io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
          io.sdramAxi.sharedCmd.len := 7 // 8 data bursts
          io.sdramAxi.writeData.last := pendingWordsCounter.willOverflowIfInc

          io.sdramAxi.writeData.valid := !pendingWordsCounter.willOverflow && !doneWritingData && dataOutFifo.io.pop.valid && sdramWrEnReg
          //          otherwise {
          //            io.sdramAxi.writeData.strb := "1111"
          //          }

          when (io.sdramAxi.writeData.fire) {
            pendingWordsCounter.increment()
            when (pendingWordsCounter.willOverflowIfInc) {
              doneWritingData := True
            }
          }

          when (doneWritingData) {
            nextNonceTagBlockFifo.io.push.valid := dataOutFifo.io.pop.valid
            nextNonceTagBlockFifo.io.push.payload := dataOutFifo.io.pop.data
            dataOutFifo.io.pop.ready := nextNonceTagBlockFifo.io.push.ready
          }

          when (dataOutFifo.io.pop.fire) {
            sdramAxiSharedCmdValidReg := True
            sdramWrEnReg := True
            goto(writeNewTagState)
          }

        onExit {
          pendingWordsCounter := 0
        }
//          sdramWrEnReg := False
//          sdramAxiSharedCmdValidReg := False
        }
      }


      val writeNewTagState: State = new State {
        whenIsActive {
          val tempNonceReg = Vec(Reg(Bits(axiConfig.dataWidth bits)), 2)
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }
          io.sdramAxi.sharedCmd.addr := getCurrentTagNodeAddr()
          io.sdramAxi.sharedCmd.len := 5 // 6 * 32 == 192 bits; 6 - 1 = 5

          io.sdramAxi.writeData.valid := nextNonceTagBlockFifo.io.pop.valid && sdramWrEnReg
          io.sdramAxi.writeData.data := nextNonceTagBlockFifo.io.pop.payload.fragment
          io.sdramAxi.writeData.last := nextNonceTagBlockFifo.io.pop.payload.last
          io.sdramAxi.writeData.strb := "1111"

          when(io.sdramAxi.writeData.fire) {
            when (pendingWordsCounter.value === 0 || pendingWordsCounter.value === 1) {
              tempNonceReg(pendingWordsCounter.value) := nextNonceTagBlockFifo.io.pop.payload.fragment
            }
            pendingWordsCounter.increment()
          }

          when(io.sdramAxi.writeRsp.fire) {
            val index = getSiblingIndex()
            nonceVecReg(index << 1) := tempNonceReg(0)
            nonceVecReg(index << 1 + 1) := tempNonceReg(1)
            caesarInputCmdValidReg := True
            goto(calculateNewTagState)
            //            pendingWordsCounter.increment()
            //            sdramAxiSharedCmdValidReg := True
            //            when(pendingWordsCounter.willOverflowIfInc) {
            //            }
          }
        }

        onExit {
          pendingWordsCounter := 0
        }
      }

      val calculateNewTagState: State = new State {
        val dataInFifoValidReg = RegInit(True)
        whenIsActive {
          when (caesarCtrl.io.in_cmd_stream.fire) {
            caesarInputCmdValidReg := False
          }

          dataInFifo.io.push.data.fragment := nonceVecReg(pendingWordsCounter)
          dataInFifo.io.push.data.last := pendingWordsCounter.willOverflowIfInc
          dataInFifo.io.push.valid := dataInFifoValidReg

          caesarCtrl.io.in_cmd_stream.mode := caesarCtrl.macGenMode

          when (dataInFifo.io.push.fire) {
            pendingWordsCounter.increment()

            when (pendingWordsCounter.willOverflowIfInc) {
              dataInFifoValidReg := False
            }
          }

          nextNonceTagBlockFifo.io.push.valid := caesarCtrl.io.out_stream.valid
          nextNonceTagBlockFifo.io.push.payload := caesarCtrl.io.out_stream.data
          caesarCtrl.io.out_stream.ready := nextNonceTagBlockFifo.io.push.ready

          when (caesarCtrl.io.out_stream.fire && caesarCtrl.io.out_stream.data.last && !caesarCtrl.io.out_stream.error) {
            sdramAxiSharedCmdValidReg := True
            caesarInputCmdValidReg := True
            updateToNextParentNodeAddrReg()
            goto(verifyTagFromSdramState)
          }
        }
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