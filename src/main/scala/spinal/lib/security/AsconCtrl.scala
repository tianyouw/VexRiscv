package spinal.lib.security

import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.{Counter, Fragment, master, slave}

/**
  * Created by Jiangyi on 2020-01-09.
  */

case class AsconCtrlInCmd(config: Axi4Config) extends Bundle {
  val mode = Bits(3 bits)
}

case class AsconCtrlInData(config: Axi4Config) extends Bundle {
  val data = Fragment(Bits(config.dataWidth bits))
}

case class AsconCtrlOutData(config: Axi4Config) extends Bundle {
  val data = Fragment(Bits(config.dataWidth bits))
  val error = Bool()
}

class AsconCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    val in_cmdstream = slave Stream(AsconCtrlInCmd(config))
    val in_datastream = slave Stream(AsconCtrlInData(config))
    val out_datastream = master Stream(AsconCtrlOutData(config))
  }

  val asconBus = new ascon()

  def ENCRYPT : Bits = "000"
  def DECRYPT : Bits = "001"
  def MACGEN : Bits = "010"
  def MACVERIFY : Bits = "011"
  def NOP : Bits = "100"

  val mode = Reg(Bits(io.in_cmdstream.mode.getBitsWidth bits))
  val queueOpCount = Counter(0 until 3)
  val counter = Counter(0 until 36)
  val isFinalQueue = queueOpCount === 3

  asconBus.io.CSxSI := False
  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when(io.in_cmdstream.fire) {
          mode := io.in_cmdstream.mode
        }

        goto(initialize)
      }
    }

    val initialize : State = new State {
      whenIsActive {
        asconBus.io.CSxSI := False
        when (counter.value <= 7) {
          when (io.in_datastream.fire) {
            asconBus.io.AddressxDI := (counter.value + 4).asBits
            asconBus.io.DataWritexDI := io.in_datastream.data.fragment
            asconBus.io.WExSI := True
            asconBus.io.CSxSI := True

            counter.increment()
          }
        } elsewhen (counter === 8) {
          asconBus.io.AddressxDI := 0x2
          asconBus.io.DataWritexDI := B(7 bits, 0 -> True, default -> False)
          asconBus.io.WExSI := True
          asconBus.io.CSxSI := True

          counter.increment()
        } otherwise {
          goto(associate)
        }
      }

      onExit(counter.clear())
    }

    val associate : State = new State {
      whenIsActive {
        asconBus.io.CSxSI := False
        when (counter < 2) {
          when (io.in_datastream.fire) {
            asconBus.io.AddressxDI := 0x0C | B(counter.value.asBits(0), 1 bit)
            asconBus.io.DataWritexDI := io.in_datastream.data.fragment
            asconBus.io.WExSI := True
            asconBus.io.CSxSI := True
            counter.increment()
          }
        } elsewhen (counter === 2) {
          asconBus.io.AddressxDI := 0x2
          asconBus.io.DataWritexDI := B(7 bits, 6 -> True, default -> False) // Only 64 bit associated data will be used
          asconBus.io.WExSI := True
          asconBus.io.CSxSI := True

          counter.increment()
        } otherwise {
          goto(enc_dec)
        }
      }

      onExit(counter.clear())
    }

    val enc_dec : State = new State {
      whenIsActive {
        asconBus.io.CSxSI := False

        when (counter < 2) {
          when (io.in_datastream.fire) {
            asconBus.io.AddressxDI := 0x0C | B(counter.value.asBits(0), 1 bit)
            asconBus.io.DataWritexDI := io.in_datastream.data.fragment
            asconBus.io.WExSI := True
            asconBus.io.CSxSI := True

            counter.increment()
          }
        } elsewhen (counter === 2) {
          asconBus.io.AddressxDI := 0x2
          asconBus.io.DataWritexDI := B(7 bits, 2 -> (mode === ENCRYPT && !isFinalQueue),
                                                3 -> (mode === DECRYPT && !isFinalQueue),
                                                4 -> (mode === ENCRYPT && isFinalQueue),
                                                5 -> (mode === DECRYPT && isFinalQueue),
                                                default -> False)
          asconBus.io.WExSI := True
          asconBus.io.CSxSI := True

          counter.increment()
        } otherwise {
          when (isFinalQueue) {
            goto(queueTag)
          } otherwise {
            goto(queueData)
          }
        }
      }

      onExit(counter.clear())
    }

    val queueData : State = new State {
      whenIsActive {
        asconBus.io.CSxSI := False
        when (counter < 2) {
          asconBus.io.AddressxDI := 0x0C | B(counter.value.asBits(0), 1 bit)
          asconBus.io.WExSI := False
          asconBus.io.CSxSI := True
          io.out_datastream.data.fragment := asconBus.io.DataReadxDO
          io.out_datastream.data.last := counter === 1 && isFinalQueue
          io.out_datastream.valid := True

          when (io.out_datastream.fire) {
            counter.increment()
          }
        } otherwise {
          queueOpCount.increment()
          goto(enc_dec)
        }
      }

      onExit(counter.clear())
    }

    val queueTag : State = new State {
      onEntry(queueOpCount.clear())

      whenIsActive {
        asconBus.io.CSxSI := False
        when (counter < 4) {
          asconBus.io.AddressxDI := 0x10 | B(counter.value.asBits(1 downto 0), 2 bits)
          asconBus.io.WExSI := False
          asconBus.io.CSxSI := True
          io.out_datastream.data.fragment := asconBus.io.DataReadxDO
          io.out_datastream.data.last := counter === 1 && isFinalQueue
          io.out_datastream.valid := True

          when (io.out_datastream.fire) {
            counter.increment()
          }
        } otherwise {
          goto(idle)
        }
      }
    }
  }
}
