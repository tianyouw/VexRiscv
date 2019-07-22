package spinal.lib.security

import spinal.core._
import spinal.lib.{Fragment, Stream, master, slave}
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi.{Axi4ArwUnburstified, Axi4Config}

/**
  * Created by Jiangyi on 2019-07-11.
  */

// Wrapper interface that takes in raw data in 32 bit chunks, and massages it to conform to CAESAR API
case class CAESARCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    val in_stream = slave Stream(Fragment(Bits(config.dataWidth bits)))
    val out_stream = master Stream(Fragment(Bits(config.dataWidth bits)))
    val encrypt = in Bool
  }

  val readyForInput = RegInit(True)
  val last = RegInit(False)
  val data = Reg(Bits(config.dataWidth bits))
  val outValid = RegInit(False)

  val counter = RegInit(UInt(8 bit))
  val nonce = RegInit(UInt(128 bit)) // TODO: pull this into a constant? also, we shouldn't be keeping the same nonce
  val key = UInt(128 bit) // TODO: we need a better way of getting the key

  val crypto = new CAESARInterface
  val public_in_stream = master Stream(Bits(32 bits))
  val secret_in_stream = master Stream(Bits(32 bits))
  val crypto_out_stream = slave Stream(Bits(32 bits))

  // TODO: why the hell don't these compile?
  crypto.io.pdi_valid := public_in_stream.valid
  public_in_stream.ready := crypto.io.pdi_ready
  crypto.io.pdi_data := public_in_stream.payload

  crypto.io.sdi_valid := secret_in_stream.valid
  secret_in_stream.ready := crypto.io.sdi_ready
  crypto.io.sdi_data := secret_in_stream.payload

  crypto_out_stream.valid := crypto.io.do_valid
  crypto.io.do_ready := crypto_out_stream.ready
  crypto_out_stream.payload := crypto.io.do_data

  val fsm = new StateMachine {
    val key_header = new State with EntryPoint {
      whenIsActive {
        // do the key header thing
        counter := 0
        goto(enter_key)
      }
    }

    val enter_key = new State {
      whenIsActive {
        when (counter === 128 / 8) {
          // put one segment of the key in
          counter := counter + 1
        } otherwise {
          counter := 0
          goto(activate_key)
        }
      }
    }

    val activate_key = new State {
      whenIsActive {
        // send activate key opcode
        goto(wait_for_input)
      }
    }

    val wait_for_input = new State {
      whenIsActive {
      }
    }
  }

  //io.in_stream.ready := readyForInput
  //io.out_stream.fragment := data
  //io.out_stream.last := last
  //io.out_stream.valid := outValid

  //outValid := False
  //when (io.in_stream.valid && readyForInput) {
  //  data := io.in_stream.fragment
  //  last := io.in_stream.last
  //  readyForInput := False
  //} elsewhen (io.out_stream.ready && !readyForInput) {
  //  outValid := True
  //  readyForInput := True
  //}

//  io.in_stream >/-> io.out_stream

  // TODO: Implement CAESAR spec
  // ???
}
