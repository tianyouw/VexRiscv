package spinal.lib.security

import spinal.core.{Area, Bundle, Component, cloneOf, isPow2}
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.memory.sdram._
import spinal.lib.{master, slave}
// import spinal.lib.security.CssbAESARInterface

/**
  * Created by Jiangyi on 2019-07-10.
  */

object Axi4SharedSecurityCtrl {
  def getAxiConfig(dataWidth: Int, idWidth: Int, layout: SdramLayout): Axi4Config = {
    val widthFactor = dataWidth / layout.dataWidth

      Axi4Config(
        addressWidth = layout.byteAddressWidth,
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


case class Axi4SharedSecurityCtrl(axiDataWidth: Int, axiIdWidth: Int, layout: SdramLayout, timing: SdramTimings, CAS: Int) extends Component {
  val axiConfig = Axi4SharedSecurityCtrl.getAxiConfig(axiDataWidth, axiIdWidth, layout)

  final val treeDepth = 4
  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    // val sdramAxi = master(Axi4Shared(axiConfig))
  }

  // val crypto = new CAESARInterface()
  // crypto.connectToCryptoEngineToBus(io.axi)

  // io.axi <> io.sdramAxi

  // ???

}
