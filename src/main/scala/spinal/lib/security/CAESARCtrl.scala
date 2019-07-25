package spinal.lib.security

import spinal.core._
import spinal.lib.{Counter, Fragment, Stream, master, slave}
import spinal.lib.bus.amba4.axi.{Axi4ArwUnburstified, Axi4Config}

/**
  * Created by Jiangyi on 2019-07-11.
  */
case class CAESARCtrlInCmd(config: Axi4Config) extends Bundle {
  val mode = Bits(3 bits)
}

case class CAESARCtrlInData(config: Axi4Config) extends Bundle {
  val data = Fragment(Bits(config.dataWidth bits))
}

case class CAESARCtrlOutData(config: Axi4Config) extends Bundle {
  val data = Fragment(Bits(config.dataWidth bits))
  val error = Bool()
}


// Temp Caesar wrapper interface 
case class DummyCAESARCtrl(config : Axi4Config) extends Component {
  val io = new Bundle {
    // val in_stream = slave Stream(Fragment(Bits(config.dataWidth bits)))
    // val out_stream = master Stream(Fragment(Bits(config.dataWidth bits)))
    val in_cmdstream = slave Stream(CAESARCtrlInCmd(config))
    val in_datastream = slave Stream(CAESARCtrlInData(config))
    val out_datastream = master Stream(CAESARCtrlOutData(config))
  }

  def ENCRYPT : Bits = "000"
  def DECRYPT : Bits = "001"
  def MACGEN : Bits = "010"
  def MACVERIFY : Bits = "011"
  def NOP : Bits = "100"
  final val burstLen : UInt = 8
  
  val readyForInput = RegInit(True)
  // val last = RegInit(False)
  val outTag = Reg(Bits(128 bits)) 
  val inTag = Reg(Bits(128 bits)) 
  val data = Reg(Fragment(Bits(config.dataWidth bits)))
  val currData = Reg(Fragment(Bits(config.dataWidth bits)))
  val lastData = Reg(Fragment(Bits(config.dataWidth bits)))
  val burstCntr = Counter(0 until 14)
  val last = Bool()
  

  val mode = Reg(Bits(3 bits))


  val outValid = RegInit(False)
  val inReady = RegInit(True)

  // val nonce = Reg(Bits(config.dataWidth bits)) init(1)
  val internalNonce = Reg(UInt(2*config.dataWidth bits)) init(1)
  val externalNonce = Reg(Bits(2*config.dataWidth bits)) init(0)
  val error = Bool()

  // val dataBurst = Reg(Array[Bits(config.dataWidth bits)](burstLen/2))
  // io.in_datastream.ready := readyForInput
  io.in_cmdstream.ready := inReady
  io.in_datastream.ready := inReady
  io.out_datastream.data := data
  io.out_datastream.error := error
  io.out_datastream.valid := outValid
  data.last := last

  outValid := False
  error := False
  last := False

  when(io.in_cmdstream.fire) {
    mode := io.in_cmdstream.mode
    burstCntr.clear()
    inReady := False
  } 

  switch(mode) {
    is(ENCRYPT){
      when (io.in_datastream.fire) {
        // input data burst
        currData := io.in_datastream.data
        readyForInput := False
        
        when (burstCntr === 0) { // even, first one
          outTag := B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment)
        } elsewhen ((burstCntr.value % 2 === 1) && (burstCntr.value < 8)) { // odd, sending data
          lastData := currData
        } elsewhen (burstCntr.value < 8) { // even, sending data
          outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
        } 
        data.fragment := currData.fragment ^ internalNonce(config.dataWidth-1 downto 0).asBits

      } elsewhen (io.out_datastream.fire) { 
        outValid := True
        readyForInput := True
        burstCntr.increment()

        when (burstCntr.value > 13) {
          internalNonce := internalNonce + 1
          burstCntr.clear()
        } elsewhen (burstCntr === 8) {   // sending internalNonce
          data.fragment := internalNonce(config.dataWidth-1 downto 0).asBits
        } elsewhen (burstCntr === 9) {   // sending internalNonce
          data.fragment := internalNonce(2*config.dataWidth-1 downto config.dataWidth).asBits
        } elsewhen (burstCntr === 10) {   // sending outTag
          data.fragment := outTag(config.dataWidth-1 downto 0)
        } elsewhen (burstCntr === 11) {   // sending outTag
          data.fragment := outTag(2*config.dataWidth-1 downto config.dataWidth)
        } elsewhen (burstCntr === 12) {   // sending outTag
          data.fragment := outTag(3*config.dataWidth-1 downto 2*config.dataWidth)
        } elsewhen (burstCntr === 13) {   // sending outTag
          data.fragment := outTag(4*config.dataWidth-1 downto 3*config.dataWidth)
          last := True
        } otherwise { // shouldn't go here
          burstCntr.clear()
        }
      }
    }
  
    is (DECRYPT){
      when (io.in_datastream.fire) {
        // input data burst
        currData := io.in_datastream.data
        readyForInput := False
        
        when (burstCntr === 0) { // nonce, lower bits
          externalNonce := B(0, config.dataWidth bits) ## currData.fragment
        } elsewhen (burstCntr === 1) { // nonce, upper bits
          externalNonce := externalNonce | (currData.fragment ## B(0, config.dataWidth bits))
        } elsewhen (burstCntr === 2) { // tag
          inTag := currData.fragment ## B(0, 128 - config.dataWidth bits) 
        } elsewhen (burstCntr.value < 6) {   // tag
          inTag := inTag.rotateRight(32) | (B(0, 128 - config.dataWidth bits) ## currData.fragment)
        } elsewhen (burstCntr.value < 13) {   // decrypt ciphered data, generate tag
          when (burstCntr.value % 2 === 0) {
            lastData := currData
          } elsewhen (burstCntr === 7) {
            outTag := B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment)
          } otherwise {
            outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
          }

          data.fragment := currData.fragment ^ externalNonce(config.dataWidth-1 downto 0)
        
        } elsewhen (burstCntr === 13) {   // last one
          data.fragment := currData.fragment ^ externalNonce(config.dataWidth-1 downto 0).asBits
          last := True

          outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
          when (outTag =/= inTag) {
            error := True
          }
        } otherwise { // shouldn't go here
          burstCntr.clear()
        }

      } elsewhen (io.out_datastream.fire) {
        outValid := True
        readyForInput := True
        burstCntr.increment()
        when (burstCntr.value > 13) {
          burstCntr.clear()
        }
      }
    }
    
    is (MACGEN){  
      when (io.in_datastream.fire) {
        // input data burst
        currData := io.in_datastream.data
        readyForInput := False
        
        when (burstCntr === 0) { // even, first one
          outTag := B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment)
        } elsewhen ((burstCntr.value % 2 === 1) && (burstCntr.value < 8)) { // odd, sending data
          lastData := currData
        } elsewhen (burstCntr.value < 8) { // even, sending data
          outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
        } 

      } elsewhen (io.out_datastream.fire) { 
        outValid := True
        readyForInput := True
        burstCntr.increment()

        when (burstCntr.value > 13) {
          internalNonce := internalNonce + 1
          burstCntr.clear()
        } elsewhen (burstCntr === 8) {   // sending internalNonce
          data.fragment := internalNonce(config.dataWidth-1 downto 0).asBits
        } elsewhen (burstCntr === 9) {   // sending internalNonce
          data.fragment := internalNonce(2*config.dataWidth-1 downto config.dataWidth).asBits
        } elsewhen (burstCntr === 10) {   // sending outTag
          data.fragment := outTag(config.dataWidth-1 downto 0)
        } elsewhen (burstCntr === 11) {   // sending outTag
          data.fragment := outTag(2*config.dataWidth-1 downto config.dataWidth)
        } elsewhen (burstCntr === 12) {   // sending outTag
          data.fragment := outTag(3*config.dataWidth-1 downto 2*config.dataWidth)
        } elsewhen (burstCntr === 13) {   // sending outTag
          data.fragment := outTag(4*config.dataWidth-1 downto 3*config.dataWidth)
          last := True
        } otherwise { // shouldn't go here
          burstCntr.clear()
        }
      }      
    }
    
    is (MACVERIFY){
      when (io.in_datastream.fire) {
        // input data burst
        currData := io.in_datastream.data
        readyForInput := False
        
        when (burstCntr === 0) { // even, first one
          outTag := B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment)
        } elsewhen ((burstCntr.value % 2 === 1) && (burstCntr.value < 8)) { // odd, sending data
          lastData := currData
        } elsewhen (burstCntr.value < 8) { // even, sending data
          outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
        } elsewhen (burstCntr.value < 11) {
          inTag := currData.fragment ## B(0, 128 - config.dataWidth bits) 
        } elsewhen (burstCntr === 11) {
          inTag := inTag.rotateRight(32) | (B(0, 128 - config.dataWidth bits) ## currData.fragment)
          data.fragment := currData.fragment    // don't care what you output
          last := True

          when (outTag =/= inTag) {
            error := True
          }        
        } elsewhen (burstCntr === 12) {
          externalNonce := B(0, config.dataWidth bits) ## currData.fragment
        } elsewhen (burstCntr === 13) {
          externalNonce := externalNonce | (currData.fragment ## B(0, config.dataWidth bits))
        } otherwise { // shouldn't go here
          burstCntr.clear()
        }

      } elsewhen (io.out_datastream.fire) {
        outValid := True
        readyForInput := True
        burstCntr.increment()
        when (burstCntr.value > 13) {
          burstCntr.clear()
        }
      }
    }

    default{
      // do nothing
    }
    
  }

  when (io.out_datastream.fire && io.out_datastream.data.last){
    inReady := True
  }

}






// Wrapper interface that takes in raw data in 32 bit chunks, and massages it to conform to CAESAR API
// case class CAESARCtrl(config : Axi4Config) extends Component {
//   val io = new Bundle {
//     // val in_stream = slave Stream(Fragment(Bits(config.dataWidth bits)))
//     // val out_stream = master Stream(Fragment(Bits(config.dataWidth bits)))
//     val in_stream = slave Stream(CAESARCtrlInData(config))
//     val out_stream = master Stream(CAESARCtrlOutData(config))
//   }
  
//   val readyForInput = RegInit(True)
//   // val last = RegInit(False)
//   val data = Reg(Fragment(Bits(config.dataWidth bits)))
//   val outValid = RegInit(False)
//   val error = Bool()
//   val tag = Reg(Bits(config.dataWidth/2 bits)) init(0)

//   io.in_stream.ready := readyForInput
//   io.out_stream.data := data
//   io.out_stream.error := error
//   io.out_stream.valid := outValid
//   // io.out_stream.tag := tag
  

//   outValid := False
//   error := False  // TODO: don't hardcode this
  
//   when (io.in_stream.valid && readyForInput) {
//     data := io.in_stream.data
//     readyForInput := False
//     tag := data.fragment(config.dataWidth/2 - 1 downto 0)
//   } elsewhen (io.out_stream.ready && !readyForInput) {
//     outValid := True
//     readyForInput := True
//   }

// //  io.in_stream >/-> io.out_stream

// //  val crypto = new CAESARInterface()
//   // TODO: Implement CAESAR spec
//   // ???
// }

