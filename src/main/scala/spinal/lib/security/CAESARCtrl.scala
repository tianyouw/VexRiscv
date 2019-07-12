package spinal.lib.security

import spinal.core._
import spinal.lib.{Fragment, Stream, master, slave}
import spinal.lib.bus.amba4.axi.{Axi4ArwUnburstified, Axi4Config}

/**
  * Created by Jiangyi on 2019-07-11.
  */

// Wrapper interface that takes in raw data in 32 bit chunks, and massages it to conform to CAESAR API
case class CAESARCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    val in_stream = slave Stream(Fragment(Bits(config.dataWidth bits)))
    val out_stream = master Stream(Bits(config.dataWidth bits))

    val in_last = in Bool()
    val out_last = out Bool()
  }


  val crypto = new CAESARInterface()
  // TODO: Implement CAESAR spec
  // ???
}
