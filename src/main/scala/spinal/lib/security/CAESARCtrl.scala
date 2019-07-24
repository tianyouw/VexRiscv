package spinal.lib.security

import spinal.core._
import spinal.lib.{Fragment, Stream, master, slave}
import spinal.lib.bus.amba4.axi.{Axi4ArwUnburstified, Axi4Config}

/**
  * Created by Jiangyi on 2019-07-11.
  */

object CAESARMode extends SpinalEnum { 
  val ENCODE, DECODE, MACGEN, MACVERIFY = value
}
// object CAESARMode extends Enumeration { 
//   val ENCODE, DECODE, MACGEN, MACVERIFY = Value
// }

case class CAESARCtrlInData(config: Axi4Config) extends Bundle {
  val mode = CAESARMode
  val data = Bits(config.dataWidth bits)
  val tag = Bits(config.dataWidth/2 bits)
  val nonce = Reg(UInt(config.dataWidth bits)) init(1)  
  // val encrypt_en = Bool()
}

case class CAESARCtrlOutData(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val tag = Bits(config.dataWidth/2 bits)
  val nonce = Reg(UInt(config.dataWidth bits)) init(1)
  val error = Bool()
}


// Temp Caesar wrapper interface 
case class DummyCAESARCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    // val in_stream = slave Stream(Fragment(Bits(config.dataWidth bits)))
    // val out_stream = master Stream(Fragment(Bits(config.dataWidth bits)))
    val in_stream = slave Stream(Fragment(CAESARCtrlInData(config)))
    val out_stream = master Stream(Fragment(CAESARCtrlOutData(config)))
  }
  
  val readyForInput = RegInit(True)
  val last = RegInit(False)
  val data = Reg(Bits(config.dataWidth bits))
  val rawData = Reg(Bits(config.dataWidth bits))
  val tag = Reg(Bits(config.dataWidth/2 bits)) init(0)

  val outValid = RegInit(False)

  // val nonce = Reg(Bits(config.dataWidth bits)) init(0)
  val nonce = Reg(UInt(config.dataWidth bits)) init(1)
  val error = Bool()

  io.in_stream.ready := readyForInput

  io.out_stream.fragment.data := data
  io.out_stream.fragment.error := error
  io.out_stream.fragment.tag := tag
  io.out_stream.last := last
  io.out_stream.valid := outValid

  outValid := False
  error := False  // TODO: send actual errors
  when (io.in_stream.valid && readyForInput) {
    rawData := io.in_stream.fragment.data
    
    last := io.in_stream.last
    readyForInput := False

    if (io.in_stream.fragment.mode == CAESARMode.ENCODE) {
      data := rawData ^ B(nonce)
      if (nonce % 2 == 0) {
        tag := tag | data(config.dataWidth/2 - 1 downto 0)
      } else {
        tag := tag ^ data(config.dataWidth/2 - 1 downto 0)
      }
    } else {
      data := rawData
      tag := tag
    }


  } elsewhen (io.out_stream.ready && !readyForInput) {



    outValid := True
    readyForInput := True
    
  } elsewhen (outValid) { //outValid && !io.in_stream.last) {
      nonce := nonce + 1
  }

}


// Wrapper interface that takes in raw data in 32 bit chunks, and massages it to conform to CAESAR API
case class CAESARCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    // val in_stream = slave Stream(Fragment(Bits(config.dataWidth bits)))
    // val out_stream = master Stream(Fragment(Bits(config.dataWidth bits)))
    val in_stream = slave Stream(Fragment(CAESARCtrlInData(config)))
    val out_stream = master Stream(Fragment(CAESARCtrlOutData(config)))
  }
  
  val readyForInput = RegInit(True)
  val last = RegInit(False)
  val data = Reg(Bits(config.dataWidth bits))
  val outValid = RegInit(False)
  val error = Bool()
  val tag = Reg(Bits(config.dataWidth/2 bits)) init(0)

  io.in_stream.ready := readyForInput
  io.out_stream.fragment.data := data
  io.out_stream.fragment.error := error
  io.out_stream.last := last
  io.out_stream.valid := outValid
  io.out_stream.tag := tag
  

  outValid := False
  error := False  // TODO: don't hardcode this
  
  when (io.in_stream.valid && readyForInput) {
    data := io.in_stream.fragment.data
    last := io.in_stream.last
    readyForInput := False
    tag := data(config.dataWidth/2 - 1 downto 0)
  } elsewhen (io.out_stream.ready && !readyForInput) {
    outValid := True
    readyForInput := True
  }

//  io.in_stream >/-> io.out_stream

//  val crypto = new CAESARInterface()
  // TODO: Implement CAESAR spec
  // ???
}

