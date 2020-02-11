package spinal.lib.security

import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.{Counter, Fragment, master, slave}

/**
  * Created by Jiangyi on 2020-01-09.
  */

case class AsconCtrlInCmd() extends Bundle {
  val mode = Bits(2 bits)
}

case class AsconCtrlInData(config: Axi4Config) extends Bundle {
  val data = Fragment(Bits(config.dataWidth bits))
}

case class AsconCtrlOutData(config: Axi4Config) extends Bundle {
  val data = Fragment(Bits(config.dataWidth bits))
}

class AsconFastCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    val in_cmdstream = slave Stream(AsconCtrlInCmd())
    val in_datastream = slave Stream(AsconCtrlInData(config))
    val out_datastream = master Stream(AsconCtrlOutData(config))
  }

  def resetAsconCmd(): Unit = {
    asconInitReg := False
    asconAssociateReg := False
    asconEncryptReg := False
    asconDecryptReg := False
    asconFinalEncryptReg := False
    asconFinalDecryptReg := False
  }

  val asconCore = new AsconCore(DATA_BLOCK_SIZE = 128, DATA_BUS_WIDTH = 128, ROUNDS_B = 8) // Settings for Ascon-128a

  def ENCRYPT : Bits = "00"
  def DECRYPT : Bits = "01"
  def MACGEN : Bits = "10"
  def MACVER : Bits = "11"

  val key = B"128'x1234_5678_90AB_CDEF_DEAD_BEEF_CAFE_BABE"
  val nonce = RegInit(B(1, 128 bits))
  val dataIn = Reg(Bits(128 bits))
  val nonceIn = Reg(Bits(128 bits))
  val dataOut = Reg(Bits(128 bits))
  val tagOut = Reg(Bits(128 bits))
  val mode = Reg(Bits(io.in_cmdstream.mode.getBitsWidth bits))

  val readyForDataIn = RegInit(True)
  val readyForCmdIn = RegInit(True)

  val doneReceivingNonce = RegInit(False)
  val finalDataIn = RegInit(False)
  val doneWritingNonce = RegInit(False)

  val dataInCounter = Counter(0 until 4)
  val dataOutCounter = Counter(0 until 4)

  val asconInitReg = RegInit(False)
  val asconAssociateReg = RegInit(False)
  val asconEncryptReg = RegInit(False)
  val asconDecryptReg = RegInit(False)
  val asconFinalEncryptReg = RegInit(False)
  val asconFinalDecryptReg = RegInit(False)

  asconCore.io.KeyxDI := key
  asconCore.io.NoncexDI := nonce
  asconCore.io.DataWritexDI := dataIn
  //  ascon.io.DP_WriteIODataxSI := DP_WriteIODataxS
  asconCore.io.CP_InitxSI := asconInitReg
  asconCore.io.CP_AssociatexSI := asconAssociateReg
  asconCore.io.CP_EncryptxSI := asconEncryptReg
  asconCore.io.CP_DecryptxSI := asconDecryptReg
  asconCore.io.CP_FinalEncryptxSI := asconFinalEncryptReg
  asconCore.io.CP_FinalDecryptxSI := asconFinalDecryptReg

  io.in_datastream.ready := readyForDataIn
  io.in_cmdstream.ready := readyForCmdIn

  io.out_datastream.payload.data.assignDontCare()
  io.out_datastream.valid := False

  when (io.in_datastream.fire) {
    dataIn(dataInCounter.value * config.dataWidth, config.dataWidth bits) := io.in_datastream.data.fragment
    finalDataIn := io.in_datastream.data.last

    when(dataInCounter.willOverflowIfInc) {
      when ((mode === DECRYPT || mode === MACVER) && !doneReceivingNonce) {
        nonceIn := dataIn
        nonceIn(dataInCounter.value * config.dataWidth, config.dataWidth bits) := io.in_datastream.data.fragment
        doneReceivingNonce := True
      } otherwise {
        readyForDataIn := False
      }
    }

    dataInCounter.increment()
  }

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when(io.in_cmdstream.fire) {
          mode := io.in_cmdstream.mode
          readyForCmdIn := False

          goto(initialize)
        }
      }
    }

    val initialize : State = new State {
      whenIsActive {
        when (mode === DECRYPT || mode === MACVER) {
          asconCore.io.NoncexDI := nonceIn
          asconInitReg := doneReceivingNonce
        } otherwise {
          asconInitReg := True
        }
        when (asconCore.io.CP_DonexSO) {
          resetAsconCmd()
          goto(encDec)
        }
      }
    }

    val encDec : State = new State {
      whenIsActive {
        when (!readyForDataIn) {
          asconEncryptReg := mode =/= DECRYPT && !finalDataIn
          asconDecryptReg := mode === DECRYPT && !finalDataIn
          asconFinalEncryptReg := mode =/= DECRYPT && finalDataIn
          asconFinalDecryptReg := mode === DECRYPT && finalDataIn

          dataOut := asconCore.io.IODataxDO
          when (asconCore.io.CP_DonexSO) {
            tagOut := asconCore.io.StatexDO(3) ## asconCore.io.StatexDO(4)
            readyForDataIn := True
            resetAsconCmd()
            when (mode === ENCRYPT || mode === DECRYPT) {
              goto(queueData)
            } elsewhen ((mode === MACGEN || mode === MACVER) && finalDataIn) {
              goto(queueTag)
            }
          }
        }
      }

//      onExit(counter.clear())
    }

    val queueData : State = new State {
      whenIsActive {
        when (doneWritingNonce || mode === DECRYPT) {
          io.out_datastream.payload.data.fragment := dataOut(dataOutCounter.value * config.dataWidth, config.dataWidth bits)
          io.out_datastream.payload.data.last := dataOutCounter.willOverflowIfInc && finalDataIn
          io.out_datastream.valid := True
          when(io.out_datastream.fire) {
            when(dataOutCounter.willOverflowIfInc) {
              when(finalDataIn) {
                goto(queueTag)
              } otherwise {
                goto(encDec)
              }
            }

            dataOutCounter.increment()
          }
        }
      }

      onExit(dataOutCounter.clear())
    }

    val queueTag : State = new State {
//      onEntry(queueOpCount.clear())

      whenIsActive {
        when (doneWritingNonce || mode === DECRYPT || mode === MACVER) {
          io.out_datastream.payload.data.fragment := tagOut(dataOutCounter.value * config.dataWidth, config.dataWidth bits)
          io.out_datastream.payload.data.last := dataOutCounter.willOverflowIfInc
          io.out_datastream.valid := True
          when(io.out_datastream.fire) {
            when(dataOutCounter.willOverflowIfInc) {
              goto(idle)
            }

            dataOutCounter.increment()
          }
        }
      }

      onExit {
        dataOutCounter.clear()
        when (mode === ENCRYPT || mode === MACGEN) {
          nonce := B(nonce.asUInt + 1, 128 bits)
        }
        doneWritingNonce := False
        doneReceivingNonce := False
        readyForDataIn := True
        finalDataIn := False
        readyForCmdIn := True
      }
    }
  }

  when (!doneWritingNonce && ((mode === ENCRYPT || mode === MACGEN) &&
        (fsm.isActive(fsm.initialize) || fsm.isActive(fsm.encDec) || fsm.isActive(fsm.queueData) || fsm.isActive(fsm.queueTag)))) {
    io.out_datastream.payload.data.fragment := nonce(dataOutCounter.value * config.dataWidth, config.dataWidth bits)
    io.out_datastream.payload.data.last := dataOutCounter.willOverflowIfInc
    io.out_datastream.valid := True
    when (dataOutCounter.willOverflowIfInc) {
      doneWritingNonce := True
    }

    dataOutCounter.increment()
  }
}
