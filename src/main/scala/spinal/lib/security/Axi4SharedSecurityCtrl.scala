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
  def getAxiConfig(dataWidth: Int, addressWidth: Int, idWidth: Int): Axi4Config = {
      Axi4Config(
        addressWidth = addressWidth,
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


case class Axi4SharedSecurityCtrl(axiDataWidth: Int, axiAddrWidth: Int, axiIdWidth: Int, layout : SdramLayout) extends Component {
//  val axiConfig = Axi4SharedSecurityCtrl.getAxiConfig(axiDataWidth, axiAddrWidth, axiIdWidth)
  val axiConfig = Axi4SharedSdramCtrl.getAxiConfig(axiDataWidth, axiIdWidth, layout)
  val startAddr = Reg(UInt(axiConfig.addressWidth bits))
  val writeToRam = Reg(Bool())
  val error = Bool()
  final val treeAry = 4

  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    val sdramAxi = master(Axi4Shared(axiConfig))
  }

  val caesarCtrl = CAESARCtrl(axiConfig)

  val ioAxiSharedCmd = io.axi.arw.unburstify
  val ioAxiCmd = ioAxiSharedCmd.haltWhen(ioAxiSharedCmd.write && !io.axi.writeData.valid)
  val writeRsp = cloneOf(io.axi.writeRsp)

  // Set up sdram AXI
  io.sdramAxi.sharedCmd.addr := io.axi.sharedCmd.addr
  io.sdramAxi.sharedCmd.valid := io.axi.sharedCmd.valid
  io.sdramAxi.sharedCmd.write := io.axi.sharedCmd.write
  io.sdramAxi.sharedCmd.size := io.axi.sharedCmd.size
  io.sdramAxi.sharedCmd.len  := io.axi.sharedCmd.len
  io.sdramAxi.sharedCmd.id := io.axi.sharedCmd.id
  io.sdramAxi.sharedCmd.burst := io.axi.sharedCmd.burst

  io.sdramAxi.writeData.valid := ioAxiCmd.valid
  io.sdramAxi.writeData.data := io.axi.writeData.data
  io.sdramAxi.writeData.strb := io.axi.writeData.strb
  io.sdramAxi.writeData.last := ioAxiCmd.last
//  io.sdramAxi.writeData.valid := caesarCtrl.io.out_stream.valid
//  io.sdramAxi.writeData.data := caesarCtrl.io.out_stream.fragment
//  io.sdramAxi.writeData.strb := "1111"
//  io.sdramAxi.writeData.last := caesarCtrl.io.out_stream.last

  //Write rsp branch
  writeRsp.valid := ioAxiCmd.fire && ioAxiCmd.write && ioAxiCmd.last
  writeRsp.id := ioAxiCmd.id
  writeRsp.setOKAY()
  writeRsp >-> io.axi.writeRsp

  // Read rsp branch
  io.axi.readRsp.id := ioAxiCmd.id
  io.axi.readRsp.last := io.sdramAxi.readRsp.last
  io.axi.readRsp.valid := io.sdramAxi.readRsp.valid
  io.axi.readRsp.data := io.sdramAxi.readRsp.data
  //  io.axi.readRsp.last := caesarCtrl.io.out_stream.last
//  io.axi.readRsp.valid := caesarCtrl.io.out_stream.valid
//  io.axi.readRsp.data := caesarCtrl.io.out_stream.fragment
  io.axi.readRsp.resp := io.sdramAxi.readRsp.resp

  //Readys
  io.axi.writeData.ready :=  ioAxiSharedCmd.valid && ioAxiSharedCmd.write && ioAxiCmd.ready
  io.sdramAxi.readRsp.ready := ioAxiSharedCmd.valid && !ioAxiSharedCmd.write
//                                && caesarCtrl.io.in_stream.ready
  io.sdramAxi.writeRsp.ready := io.axi.writeRsp.ready
  ioAxiCmd.ready      := io.sdramAxi.sharedCmd.ready

//  when (ioAxiSharedCmd.write) {
//    caesarCtrl.io.in_stream.valid := ioAxiCmd.write && ioAxiCmd.valid && caesarCtrl.io.in_stream.ready
//    // TODO: Implement mask based on strb
//    //    val mask = Bits(axiConfig.dataWidth bits)
//    caesarCtrl.io.in_stream.payload.fragment := io.axi.writeData.data
//    caesarCtrl.io.in_stream.payload.last := ioAxiSharedCmd.last
//    caesarCtrl.io.out_stream.ready := io.sdramAxi.writeData.ready
//
////    io.sdramAxi.writeData <> io.axi.writeData
////    io.sdramAxi.writeData.valid := caesarCtrl.io.out_stream.valid
////    io.sdramAxi.writeData.data := caesarCtrl.io.out_stream.fragment
////    io.sdramAxi.writeData.strb := io.axi.writeData.strb
////    io.sdramAxi.writeData.last := caesarCtrl.io.out_stream.last
//    error := io.sdramAxi.b.valid && !io.sdramAxi.b.isOKAY()
//  } otherwise {
//    caesarCtrl.io.in_stream.valid := !io.sdramAxi.sharedCmd.write && io.sdramAxi.readRsp.valid && caesarCtrl.io.in_stream.ready
//    caesarCtrl.io.in_stream.payload.fragment := io.sdramAxi.readRsp.data
//    caesarCtrl.io.in_stream.payload.last := io.sdramAxi.readRsp.last
//
//    caesarCtrl.io.out_stream.ready := io.axi.readRsp.ready
//
//
//    error := io.axi.b.valid && !io.axi.b.isOKAY()
//  }

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
