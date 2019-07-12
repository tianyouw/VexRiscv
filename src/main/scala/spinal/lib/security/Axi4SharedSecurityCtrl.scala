package spinal.lib.security

import spinal.core._
import spinal.core.internals.Operator
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.memory.sdram._
import spinal.lib.{Fragment, Stream, master, slave}

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

  val startAddr = Reg(UInt(axiConfig.addressWidth bits))
  val writeToRam = Reg(Bool())
  val error = Bool()
  final val treeAry = 4

  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    val sdramAxi = master(Axi4Shared(axiConfig))
  }


  val ioAxiSharedCmd = io.axi.arw.unburstify
  val ioAxiCmd = ioAxiSharedCmd.haltWhen(ioAxiSharedCmd.write && !io.axi.writeData.valid)

  when (ioAxiSharedCmd.isFirst) {
    writeToRam := ioAxiCmd.write
  }

  val caesarCtrl = CAESARCtrl(axiConfig)

  when (writeToRam) {
    caesarCtrl.io.in_stream.valid := ioAxiCmd.write && ioAxiCmd.valid && caesarCtrl.io.in_stream.ready
    caesarCtrl.io.in_stream.payload.fragment := io.axi.writeData.data & io.axi.writeData.strb
    caesarCtrl.io.in_stream.payload.last := ioAxiSharedCmd.last

    io.sdramAxi.sharedCmd := cloneOf(io.axi.arw)
    io.sdramAxi.writeData.valid := caesarCtrl.io.out_stream.valid
    io.sdramAxi.writeData.data := caesarCtrl.io.out_stream.payload
    caesarCtrl.io.out_stream.ready := io.sdramAxi.writeData.ready

    io.sdramAxi.b.ready := True

    error := io.sdramAxi.b.valid && !io.sdramAxi.b.isOKAY()
  } otherwise {
    io.sdramAxi.sharedCmd := cloneOf(io.axi.arw)

    val sdramAxiSharedCmd = io.sdramAxi.arw.unburstify
    val sdramAxiCmd = sdramAxiSharedCmd.haltWhen(!sdramAxiSharedCmd.write && !io.sdramAxi.writeData.valid) // Halt when it's a read and the read is not valid

    caesarCtrl.io.in_stream.valid := !sdramAxiCmd.write && io.axi.writeData.valid && caesarCtrl.io.in_stream.ready
    caesarCtrl.io.in_stream.payload.fragment := io.sdramAxi.readRsp.data
    caesarCtrl.io.in_stream.payload.last := io.sdramAxi.readRsp.last

    io.axi.writeData.valid := caesarCtrl.io.out_stream.valid
    io.axi.writeData.data := caesarCtrl.io.out_stream.payload
    caesarCtrl.io.out_stream.ready := io.axi.writeData.ready

    io.axi.b.ready := True

    error := io.axi.b.valid && !io.axi.b.isOKAY()
  }

  when (io.axi.sharedCmd.valid) {
    // Used for calculating memory tree lookup
    startAddr := io.axi.sharedCmd.payload.addr
  }


  // when (io.axi.sharedCmd.payload.write) {
  //   ???
  // } otherwise() {
  //   // Read command
  //   ???
  // }

  // ???

}
