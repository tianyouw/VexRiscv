package spinal.lib.security

import spinal.core._
import spinal.lib.{Fragment, Stream, master, slave}
import spinal.lib.bus.amba4.axi.{Axi4ArwUnburstified, Axi4Config}

/**
  * Created by Jiangyi on 2019-07-11.
  */

case class CAESARCtrlInData(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val encrypt_en = Bool()
}

case class CAESARCtrlOutData(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val error = Bool()
}

// Wrapper interface that takes in raw data in 32 bit chunks, and massages it to conform to CAESAR API
case class CAESARCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    val in_stream = slave Stream(Fragment(CAESARCtrlInData(config)))
    val out_stream = master Stream(Fragment(CAESARCtrlOutData(config)))
  }

  val readyForInput = RegInit(True)
  val last = RegInit(False)
  val data = Reg(Bits(config.dataWidth bits))
  val outValid = RegInit(False)
  io.in_stream.ready := readyForInput
  io.out_stream.fragment.data := data
  io.out_stream.last := last
  io.out_stream.valid := outValid
  io.out_stream.error := False

  outValid := False
  when (io.in_stream.valid && readyForInput) {
    data := io.in_stream.fragment.data
    last := io.in_stream.last
    readyForInput := False
  } elsewhen (io.out_stream.ready && !readyForInput) {
    outValid := True
    readyForInput := True
  }

//  io.in_stream >/-> io.out_stream

//  val crypto = new CAESARInterface()
  // TODO: Implement CAESAR spec
  // ???
}
