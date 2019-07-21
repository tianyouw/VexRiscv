package spinal.lib.security

import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4Arw, Axi4Config, Axi4Shared}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.memory.sdram.{Axi4SharedSdramCtrl, SdramLayout}
import spinal.lib.{Counter, CounterUpDown, Fragment, StreamFifo, master, slave}

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
  def bytePosition(addr: UInt) : UInt = addr(4 downto 2)

  def combineStrbDataWithOriginal(originalData: Bits, newData: Bits, strbMask: Bits) : Bits = {
    assert(originalData.getWidth == newData.getWidth)
    assert(newData.getWidth == 32)
    val outData = originalData

    if (strbMask(3) == True) {
      outData(31 downto 24) := newData(31 downto 24)
    }

    if (strbMask(2) == True) {
      outData(23 downto 16) := newData(23 downto 16)
    }

    if (strbMask(1) == True) {
      outData(15 downto 8) := newData(15 downto 8)
    }

    if (strbMask(0) == True) {
      outData(7 downto 0) := newData(7 downto 0)
    }

    outData
  }

  def getSdramDataAddress(originalAddr: UInt, axiConfig: Axi4Config): UInt = {
    originalAddr(axiConfig.addressWidth - 1 downto 5) @@ U"00000"
  }

  // Assuming 4-ary for now
  def getTreeLeafNodeTagAddress(originalAddr: UInt, axiConfig: Axi4Config): UInt = {
    0x4000000 + (getSdramDataAddress(originalAddr, axiConfig) / 32) * 24
  }

  def getParentNodeStartAddress(currentAddr: UInt, axiConfig: Axi4Config, levelIndex: Int): UInt = {
    // Floor to nearest aligned memory address (Every 3 bytes is one mem tree block), add offset into the next level
    (currentAddr / 12) * 12 + (0x300000 >> (2 * levelIndex))
  }



  val writeToRam = Reg(Bool())
  val error = Bool()
  final val burstLen = 8

  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    val sdramAxi = master(Axi4Shared(axiConfig))
  }

  val dataInFifo = StreamFifo(dataType = Fragment(CAESARCtrlInData(axiConfig)), depth = 10) // Data = 8, 2 more for nonce
  val dataOutFifo = StreamFifo(dataType = Fragment(CAESARCtrlOutData(axiConfig)), depth = 14) // Data = 8 bursts, 6 more for tag + nonce

  val pendingWordsCounter = Counter(0 until burstLen)
  val caesarCtrl = CAESARCtrl(axiConfig)

//  val sharedCmdReg = RegNextWhen(io.axi.sharedCmd, io.axi.sharedCmd.fire)

  val sdramWrEnReg = RegInit(False)
  val writeRspValidReg = RegInit(False)
  val axiSharedCmdReadyReg = RegInit(True)
  val sdramAxiSharedCmdValidReg = RegInit(False)
  val readFinishedReg = RegInit(True)
  val addrReg = Reg(UInt(axiConfig.addressWidth bits))
  val dataReg = Reg(Bits(axiDataWidth bits))
  val strbReg = Reg(Bits(axiConfig.bytePerWord bits))
  val busyReg = RegInit(False)

  dataInFifo.io.push.valid := False
  dataInFifo.io.push.payload.assignDontCare()

  caesarCtrl.io.out_stream <> dataOutFifo.io.push
  caesarCtrl.io.in_stream <> dataInFifo.io.pop

  dataOutFifo.io.pop.ready := False
  when (busyReg || io.axi.sharedCmd.write) {
    dataOutFifo.io.pop.ready := io.sdramAxi.writeData.ready

    io.sdramAxi.writeData.valid := dataOutFifo.io.pop.valid && sdramWrEnReg
    io.sdramAxi.writeData.data := dataOutFifo.io.pop.payload.fragment.data
    io.sdramAxi.writeData.strb := "1111"
    io.sdramAxi.writeData.last := dataOutFifo.io.pop.payload.last

    io.sdramAxi.sharedCmd.size := io.axi.sharedCmd.size
    io.sdramAxi.sharedCmd.len  := burstLen - 1
    io.sdramAxi.sharedCmd.id := io.axi.sharedCmd.id
    io.sdramAxi.sharedCmd.burst := io.axi.sharedCmd.burst
    io.sdramAxi.sharedCmd.write := sdramWrEnReg
    io.sdramAxi.sharedCmd.addr := getSdramDataAddress(addrReg, axiConfig)
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
    val writeFsm = new StateMachine {
      val idleState: State = new State with EntryPoint {
        onEntry {
          pendingWordsCounter := 0
          sdramWrEnReg := False
        }

        whenIsActive {
          when(io.axi.sharedCmd.fire && io.axi.writeData.fire) {
            writeRspValidReg := True
            busyReg := True
            addrReg := io.axi.sharedCmd.addr
            dataReg := io.axi.writeData.data
            strbReg := io.axi.writeData.strb
            io.axi.writeRsp.isOKAY()
          }

          when(io.axi.writeRsp.fire) {
            writeRspValidReg := False
            goto(readState)
          }
        }

        onExit {
          sdramAxiSharedCmdValidReg := True
        }
      }

      val readState: State = new State {
        //    onEntry(pendingWordsCounter := 0)
        whenIsActive {
            when (io.sdramAxi.sharedCmd.fire) {
              sdramAxiSharedCmdValidReg := False
            }

            io.sdramAxi.writeData.valid := False
//          val axiSharedCmd = cloneOf(io.axi.sharedCmd)
//
//          axiSharedCmd.addr := io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
//          axiSharedCmd.valid := True
//          axiSharedCmd.write := False
//
//          io.sdramAxi.sharedCmd <> axiSharedCmd

          dataInFifo.io.push.payload.last := io.sdramAxi.readRsp.last
          dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid

//          when(pendingWordsCounter.value === bytePosition(addrReg)) {
//            dataInFifo.io.push.payload.fragment := dataReg
//          } otherwise {
          dataInFifo.io.push.payload.fragment.data := io.sdramAxi.readRsp.data
          dataInFifo.io.push.payload.fragment.encrypt_en := False // This is a decryption
//          }


          when(io.sdramAxi.readRsp.fire) {
//            io.sdramAxi.sharedCmd.valid := False
            pendingWordsCounter.increment()
//            sdramAxiSharedCmdValidReg := True
            when(pendingWordsCounter.willOverflowIfInc) {
              goto(reEncryptState)
            }
          }
        }
      }

      val reEncryptState: State = new State {
        whenIsActive {
          when(pendingWordsCounter.value === bytePosition(addrReg)) {
            dataInFifo.io.push.payload.fragment.data := combineStrbDataWithOriginal(dataOutFifo.io.pop.payload.fragment.data, dataReg, strbReg)
          } otherwise {
            dataInFifo.io.push.payload.fragment.data := dataOutFifo.io.pop.payload.fragment.data
          }
          dataInFifo.io.push.valid := dataOutFifo.io.pop.valid
          dataInFifo.io.push.payload.fragment.encrypt_en := True // This is an encryption
          dataOutFifo.io.pop.ready := dataInFifo.io.push.ready
        }

        when(dataInFifo.io.push.fire) {
          //            io.sdramAxi.sharedCmd.valid := False
          pendingWordsCounter.increment()
          //            sdramAxiSharedCmdValidReg := True
          when(pendingWordsCounter.willOverflowIfInc) {
            goto(writeDataState)
          }
        }
        onExit {
          sdramWrEnReg := True
          sdramAxiSharedCmdValidReg := True
        }
      }

      val writeDataState: State = new State {
        whenIsActive {
          when (io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

//          when(pendingWordsCounter.value === bytePosition(addrReg)) {
//            io.sdramAxi.writeData.strb := strbReg
//          }
//          otherwise {
//            io.sdramAxi.writeData.strb := "1111"
//          }

          when(io.sdramAxi.writeRsp.fire) {
            pendingWordsCounter.increment()
//            sdramAxiSharedCmdValidReg := True
            when(pendingWordsCounter.willOverflowIfInc) {
              goto(idleState)
            }
          }
        }

        onExit {
//          sdramWrEnReg := False
//          sdramAxiSharedCmdValidReg := False
//          busyReg := False
          sdramAxiSharedCmdValidReg := True
        }
      }

      val writeTagState: State = new State {
        whenIsActive {
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          //          when(pendingWordsCounter.value === bytePosition(addrReg)) {
          //            io.sdramAxi.writeData.strb := strbReg
          //          }
          //          otherwise {
          //            io.sdramAxi.writeData.strb := "1111"
          //          }

          when(io.sdramAxi.writeRsp.fire) {
            pendingWordsCounter.increment()
            //            sdramAxiSharedCmdValidReg := True
            when(pendingWordsCounter.willOverflowIfInc) {
              goto(idleState)
            }
          }
        }
      }
    }

  } otherwise {
    io.axi <> io.sdramAxi
    when (io.axi.sharedCmd.fire) {
      readFinishedReg := False
    }
    when (io.axi.readRsp.fire && io.axi.readRsp.last) {
      readFinishedReg := True
    }

  }

//  when (io.axi.sharedCmd.valid) {
//    when (io.axi.sharedCmd.write) {
//      io.sdramAxi.sharedCmd.addr := io.axi.sharedCmd.addr(axiDataWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
//      io.sdramAxi.sharedCmd.valid := io.axi.sharedCmd.valid && (pendingWordsCounter.value =/= bytePosition(io.axi.sharedCmd.addr) || sdramWrEnReg)
//      io.sdramAxi.sharedCmd.write := sdramWrEnReg
//      io.sdramAxi.sharedCmd.size := io.axi.sharedCmd.size
//      io.sdramAxi.sharedCmd.len  := io.axi.sharedCmd.len
//      io.sdramAxi.sharedCmd.id := io.axi.sharedCmd.id
//      io.sdramAxi.sharedCmd.burst := io.axi.sharedCmd.burst
//
//      when (pendingWordsCounter.willOverflowIfInc) {
//        sdramWrEnReg := !sdramWrEnReg
//      }
//
//      when(dataInFifo.io.push.ready &&
//        ((sdramWrEnReg && io.sdramAxi.writeData.fire) ||
//          (!sdramWrEnReg && (io.sdramAxi.readRsp.fire || (pendingWordsCounter.value === bytePosition(io.axi.sharedCmd.addr)))))) {
//        pendingWordsCounter.increment()
//      }
//
//      io.axi.sharedCmd.ready := io.sdramAxi.writeRsp.valid
//      io.axi.writeData.ready := io.sdramAxi.writeRsp.valid
//    } otherwise {
//      io.axi <> io.sdramAxi
//    }
//  }

}