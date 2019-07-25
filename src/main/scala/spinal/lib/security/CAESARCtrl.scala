package spinal.lib.security

import spinal.core._
import spinal.lib.{Fragment, Stream, master, slave}
import spinal.lib.bus.amba4.axi.{Axi4ArwUnburstified, Axi4Config}

/**
  * Created by Jiangyi on 2019-07-11.
  */

case class CAESARCtrlInCmd(config: Axi4Config) extends Bundle {
  val mode = in Bits(2 bits)
}

case class CAESARCtrlInData(config: Axi4Config) extends Bundle {
  val data = Fragment(Bits(config.dataWidth bits))
}

case class CAESARCtrlOutData(config: Axi4Config) extends Bundle {
  val data = Fragment(Bits(config.dataWidth bits))
  val error = Bool()
}

// Wrapper interface that takes in raw data in 32 bit chunks, and massages it to conform to CAESAR API
case class CAESARCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    val in_cmd_stream = slave Stream(CAESARCtrlInCmd(config))
    val in_data_stream = slave Stream(CAESARCtrlInData(config))
    val out_stream = master Stream(CAESARCtrlOutData(config))
  }

  def encMode: Bits = "00"
  def decMode: Bits = "01"
  def macGenMode: Bits = "10"
  def macVerMode: Bits = "11"

  val readyForInput = RegInit(True)
  val last = RegInit(False)
  val data = Reg(Bits(config.dataWidth bits))
  val outValid = RegInit(False)
  io.in_stream.ready := readyForInput
  io.out_stream.fragment := data
  io.out_stream.last := last
  io.out_stream.valid := outValid

  outValid := False
  when (io.in_stream.valid && readyForInput) {
    data := io.in_stream.fragment
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
