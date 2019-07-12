package spinal.lib.security

import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4Shared
import spinal.lib.memory.sdram.SdramCtrlAxi4SharedContext
import spinal.lib.{IMasterSlave, master}

/**
  * Created by Jiangyi on 2019-07-10.
  */

/**
  See Figure 2 of : https://pdfs.semanticscholar.org/35a9/27d06d84d3f4079bde468cd74505235eb5e0.pdf
  */

class CAESARInterface() extends BlackBox {

  val io = new Bundle {
    val clk = in Bool
    val rst = in Bool

    // Public data ports
    val pdi_data = in Bits(32 bits)
    val pdi_valid = in Bool
    val pdi_ready = out Bool

    // Secret data ports
    val sdi_data = in Bits(32 bits)
    val sdi_valid = in Bool
    val sdi_ready = out Bool

    // Public data ports
    val do_data = out Bits(32 bits)
    val do_valid = out Bool
    val do_ready = in Bool
  }

  noIoPrefix()
  mapClockDomain(clock=io.clk, reset = io.rst)
  // TODO: Add Ascon RTL source here
  ???

}