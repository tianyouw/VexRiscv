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

  val asconCore = new AsconCore(DATA_BLOCK_SIZE = 128, DATA_BUS_WIDTH = 128)

  def ENCRYPT : Bits = "00"
  def DECRYPT : Bits = "01"
  def MACGEN : Bits = "10"
  def MACVER : Bits = "11"

  val key = B"128'x1234_5678_90AB_CDEF_DEAD_BEEF_CAFE_BABE"
  val nonce = B(1, 128 bits)
  val dataIn = Reg(B(0, 128 bits))
  val nonceIn = Reg(B(0, 128 bits))
  val dataOut = Reg(B(0, 128 bits))
  val tagOut = Reg(B(0, 128 bits))
  val mode = Reg(Bits(io.in_cmdstream.mode.getBitsWidth bits))

  val readyForDataIn = RegInit(True)
  val readyForCmdIn = RegInit(True)

  val doneReceivingNonce = RegInit(False)
  val finalDataIn = RegInit(False)
  val doneWritingNonce = RegInit(False)

  val dataInCounter = Counter(0 until 4)
  val dataOutCounter = Counter(0 until 4)

  asconCore.io.KeyxDI := key
  asconCore.io.NoncexDI := nonce
  asconCore.io.DataWritexDI := dataIn
  //  ascon.io.DP_WriteIODataxSI := DP_WriteIODataxS
  asconCore.io.CP_InitxSI := False
  asconCore.io.CP_AssociatexSI := False
  asconCore.io.CP_EncryptxSI := False
  asconCore.io.CP_DecryptxSI := False
  asconCore.io.CP_FinalEncryptxSI := False
  asconCore.io.CP_FinalDecryptxSI := False

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

  when (io.in_cmdstream.fire || dataOutCounter.value > 0) {
    io.out_datastream.payload.data.fragment := nonce(dataOutCounter.value * config.dataWidth, config.dataWidth bits)
    io.out_datastream.payload.data.last := dataOutCounter.willOverflowIfInc

    when (dataOutCounter.willOverflowIfInc) {
      doneWritingNonce := True
    }

    dataOutCounter.increment()
  }

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when(io.in_cmdstream.fire) {
          mode := io.in_cmdstream.mode
          readyForCmdIn := False
//          asconCore.io.CP_InitxSI := True
        }

        goto(initialize)
      }
    }

    val initialize : State = new State {
      whenIsActive {
        when (mode === DECRYPT || mode === MACVER) {
          asconCore.io.NoncexDI := nonceIn
          asconCore.io.CP_InitxSI := doneReceivingNonce
        } otherwise {
          asconCore.io.CP_InitxSI := True
        }
        when (asconCore.io.CP_DonexSO) {
          asconCore.io.CP_InitxSI := False
          goto(encDec)
        }
      }
    }

    val encDec : State = new State {
      whenIsActive {
        asconCore.io.CP_EncryptxSI := False
        asconCore.io.CP_DecryptxSI := False
        asconCore.io.CP_FinalEncryptxSI :=  False
        asconCore.io.CP_FinalDecryptxSI :=  False
        when (!readyForDataIn) {
          asconCore.io.CP_EncryptxSI := mode =/= DECRYPT && !finalDataIn
          asconCore.io.CP_DecryptxSI := mode === DECRYPT && !finalDataIn
          asconCore.io.CP_FinalEncryptxSI := mode =/= DECRYPT && finalDataIn
          asconCore.io.CP_FinalDecryptxSI := mode === DECRYPT && finalDataIn

          when (asconCore.io.CP_DonexSO) {

            dataOut := asconCore.io.IODataxDO
            tagOut := asconCore.io.StatexDO(3) ## asconCore.io.StatexDO(4)
            readyForDataIn := True
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
        when (doneWritingNonce) {
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
        when (doneWritingNonce) {
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
        nonce := B(nonce.asUInt + 1, 128 bits)
        doneWritingNonce := False
        readyForDataIn := True
        finalDataIn := False
        readyForCmdIn := True
      }
    }
  }
}
