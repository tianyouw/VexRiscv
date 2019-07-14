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
  def bytePosition(addr: UInt) : UInt = addr(4 downto 2)


  val axiConfig = Axi4SharedSdramCtrl.getAxiConfig(axiDataWidth, axiIdWidth, layout)
  val startAddr = Reg(UInt(axiConfig.addressWidth bits))
  val writeToRam = Reg(Bool())
  val error = Bool()
  final val treeAry = 4

  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    val sdramAxi = master(Axi4Shared(axiConfig))
  }
  val dataInFifo = StreamFifo(dataType = Fragment(Bits(axiDataWidth bits)), depth = 8)
  val dataOutFifo = StreamFifo(dataType = Fragment(Bits(axiDataWidth bits)), depth = 9) // Data = 8 bursts, One more for tag

  val pendingWordsCounter = Counter(0 to 7)
  val caesarCtrl = CAESARCtrl(axiConfig)

//  val sharedCmdReg = RegNextWhen(io.axi.sharedCmd, io.axi.sharedCmd.fire)

  val sdramWrEnReg = RegInit(False)
  val writeRspValidReg = RegInit(False)
  val axiSharedCmdValidReg = RegInit(False)
  val sdramAxiSharedCmdValidReg = RegInit(False)
  val readFinishedReg = RegInit(True)


  dataInFifo.io.push.valid := False
  dataInFifo.io.push.payload.assignDontCare()

  caesarCtrl.io.out_stream <> dataOutFifo.io.push
  caesarCtrl.io.in_stream <> dataInFifo.io.pop

  when (io.axi.sharedCmd.write && readFinishedReg) {
    dataOutFifo.io.pop.ready := io.sdramAxi.writeData.ready

    io.sdramAxi.writeData.valid := dataOutFifo.io.pop.valid && sdramWrEnReg
    io.sdramAxi.writeData.data := dataOutFifo.io.pop.payload.fragment
    io.sdramAxi.writeData.strb := io.axi.writeData.strb
    io.sdramAxi.writeData.last := dataOutFifo.io.pop.payload.last

    io.sdramAxi.sharedCmd.size := io.axi.sharedCmd.size
    io.sdramAxi.sharedCmd.len  := 0
    io.sdramAxi.sharedCmd.id := io.axi.sharedCmd.id
    io.sdramAxi.sharedCmd.burst := io.axi.sharedCmd.burst
    io.sdramAxi.sharedCmd.write := sdramWrEnReg
    io.sdramAxi.sharedCmd.addr := io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
//    io.sdramAxi.sharedCmd.addr := io.axi.sharedCmd.addr
    io.sdramAxi.sharedCmd.valid := sdramAxiSharedCmdValidReg
    io.sdramAxi.writeRsp.ready := True


    io.axi.sharedCmd.ready := axiSharedCmdValidReg
    io.axi.writeData.ready := False
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
          when(io.axi.sharedCmd.fire) {
            writeRspValidReg := True
            axiSharedCmdValidReg := False

            io.axi.writeData.ready := True
            io.axi.writeRsp.isOKAY()
          }

          when(io.axi.writeRsp.fire) {
            writeRspValidReg := False
          }

          when(io.axi.sharedCmd.valid && !io.axi.sharedCmd.ready) {
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

          dataInFifo.io.push.payload.last := True

          dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid
          when(pendingWordsCounter.value === bytePosition(io.axi.sharedCmd.addr)) {
            dataInFifo.io.push.payload.fragment := io.axi.writeData.data
          } otherwise {
            dataInFifo.io.push.payload.fragment := io.sdramAxi.readRsp.data
          }


          when(dataInFifo.io.push.ready && (io.sdramAxi.readRsp.fire)) {
//            io.sdramAxi.sharedCmd.valid := False
            pendingWordsCounter.increment()
            sdramAxiSharedCmdValidReg := True
            when(pendingWordsCounter.willOverflowIfInc) {
              goto(writeState)
            }
          }
        }

        onExit(sdramWrEnReg := True)
      }

      val writeState: State = new State {
        whenIsActive {
          when (io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          when(io.sdramAxi.writeRsp.fire) {
            pendingWordsCounter.increment()
            sdramAxiSharedCmdValidReg := True
            when(pendingWordsCounter.willOverflowIfInc) {
              goto(idleState)
            }
          }
        }

        onExit {
          sdramWrEnReg := False
          axiSharedCmdValidReg := True
          sdramAxiSharedCmdValidReg := False
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
    dataOutFifo.io.pop.ready := False

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