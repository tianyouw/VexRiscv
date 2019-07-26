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

  val readyForInput = RegInit(False)
  // val last = RegInit(False)
  val outTag = Reg(Bits(128 bits))
  val inTag = Reg(Bits(128 bits))
  val data = Reg(Fragment(Bits(config.dataWidth bits)))
  val currData = Fragment(Bits(config.dataWidth bits))
  val lastData = Reg(Fragment(Bits(config.dataWidth bits)))
  val burstCntr = Counter(0 until 24)
  val last = Bool()


  val mode = Reg(Bits(3 bits))


  val outValid = RegInit(False)
  val inReady = RegInit(True)

  // val nonce = Reg(Bits(config.dataWidth bits)) init(1)
  val internalNonce = Reg(UInt(4*config.dataWidth bits)) init(1)
  val externalNonce = Reg(Bits(4*config.dataWidth bits)) init(0)
  val error = Bool()

  // val dataBurst = Reg(Array[Bits(config.dataWidth bits)](burstLen/2))
  // io.in_datastream.ready := readyForInput
  io.in_cmdstream.ready := inReady
  io.in_datastream.ready := readyForInput
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
    readyForInput := True
  }
  currData.assignDontCare()

  switch(mode) {
    is(ENCRYPT){
      when (io.in_datastream.fire && burstCntr < 8) {
        // input data burst
        currData := io.in_datastream.data

        when ((burstCntr.value % 2 === 0)) { // odd, sending data
          lastData := currData
        } elsewhen (burstCntr === 1) { // even, first one
          outTag := B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment)
        } elsewhen (burstCntr.value < 8) { // even, sending data
          outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
        }
        data.fragment := currData.fragment ^ internalNonce(config.dataWidth-1 downto 0).asBits

        outValid := True
        readyForInput := False

      } elsewhen (io.out_datastream.fire) {
        outValid := False
        readyForInput := True
        
        burstCntr.increment()
        when (burstCntr === 15) {
          burstCntr.clear()
          internalNonce := internalNonce + 1
          inReady := True
        } 

      } elsewhen (burstCntr >= 8) {
        when (burstCntr === 8) {   // sending internalNonce
          data.fragment := internalNonce(4*config.dataWidth-1 downto 3*config.dataWidth).asBits
        } elsewhen (burstCntr === 9) {   // sending internalNonce
          data.fragment := internalNonce(3*config.dataWidth-1 downto 2*config.dataWidth).asBits
        } elsewhen (burstCntr === 10) {   // sending internalNonce
          data.fragment := internalNonce(2*config.dataWidth-1 downto config.dataWidth).asBits
        } elsewhen (burstCntr === 11) {   // sending internalNonce
          data.fragment := internalNonce(config.dataWidth-1 downto 0).asBits
        } elsewhen (burstCntr === 12) {   // sending outTag
          data.fragment := outTag(4*config.dataWidth-1 downto 3*config.dataWidth)
        } elsewhen (burstCntr === 13) {   // sending outTag
          data.fragment := outTag(3*config.dataWidth-1 downto 2*config.dataWidth)
        } elsewhen (burstCntr === 14) {   // sending outTag
          data.fragment := outTag(2*config.dataWidth-1 downto config.dataWidth) 
        } elsewhen (burstCntr === 15) {   // sending outTag
          data.fragment := outTag(config.dataWidth-1 downto 0)
          last := True
        }

        outValid := True
        readyForInput := False

      }
    }

    is (DECRYPT){
      when (io.in_datastream.fire) {
        // input data burst
        currData := io.in_datastream.data

        when (burstCntr === 0) { // nonce
          externalNonce := B(0, 128 - config.dataWidth bits) ## currData.fragment
        } elsewhen (burstCntr < 4) { // nonce
          externalNonce := externalNonce.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## currData.fragment)
        } elsewhen (burstCntr === 4) { // tag
          inTag := B(0, 128 - config.dataWidth bits) ## currData.fragment
        } elsewhen (burstCntr.value < 8) {   // tag
          inTag := inTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## currData.fragment)
        } elsewhen (burstCntr.value < 15) {   // decrypt ciphered data, generate tag
          when (burstCntr.value % 2 === 0) {
            lastData := currData
          } elsewhen (burstCntr === 9) {
            outTag := B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment)
          } otherwise {
            outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
          }

          data.fragment := currData.fragment ^ externalNonce(config.dataWidth-1 downto 0)

        } elsewhen (burstCntr === 15) {   // last one
          data.fragment := currData.fragment ^ externalNonce(config.dataWidth-1 downto 0).asBits
          last := True

          outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
          when (outTag =/= inTag) {
            error := True
          }
        } otherwise { // shouldn't go here
          burstCntr.clear()
        }

        when (burstCntr < 8) {
          burstCntr.increment()
        } otherwise {
          outValid := True
          readyForInput := False
        }

      } elsewhen (io.out_datastream.fire) {
        burstCntr.increment()
        when (burstCntr === 15) {
          burstCntr.clear()
          inReady := True
        } 

        outValid := False
        readyForInput := True
      }
    }
    
    is (MACGEN){  
      when (io.in_datastream.fire) {
        // input data burst
        currData := io.in_datastream.data

        when (burstCntr.value === 3) {
          outTag := B(0, 128 - config.dataWidth bits) ## currData.fragment
        } elsewhen (burstCntr.value % 4 === 3) {
          outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## currData.fragment)
        } otherwise {
          // do nothing
        }

        // when ((burstCntr.value % 2 === 0)) { // odd, sending data
        //   lastData := currData
        // } elsewhen (burstCntr === 1) { // even, first one
        //   outTag := B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment)
        // } elsewhen (burstCntr.value < 8) { // even, sending data
        //   outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
        // }

        burstCntr.increment()
        
      } elsewhen (burstCntr >= 16) {
        when (burstCntr === 16) {   // sending internalNonce
          data.fragment := internalNonce(4*config.dataWidth-1 downto 3*config.dataWidth).asBits
        } elsewhen (burstCntr === 17) {   // sending internalNonce
          data.fragment := internalNonce(3*config.dataWidth-1 downto 2*config.dataWidth).asBits
        } elsewhen (burstCntr === 18) {   // sending internalNonce
          data.fragment := internalNonce(2*config.dataWidth-1 downto config.dataWidth).asBits
        } elsewhen (burstCntr === 19) {   // sending internalNonce
          data.fragment := internalNonce(config.dataWidth-1 downto 0).asBits
        } elsewhen (burstCntr === 20) {   // sending outTag
          data.fragment := outTag(4*config.dataWidth-1 downto 3*config.dataWidth)
        } elsewhen (burstCntr === 21) {   // sending outTag
          data.fragment := outTag(3*config.dataWidth-1 downto 2*config.dataWidth)
        } elsewhen (burstCntr === 22) {   // sending outTag
          data.fragment := outTag(2*config.dataWidth-1 downto config.dataWidth)
        } elsewhen (burstCntr === 23) {   // sending outTag
          data.fragment := outTag(config.dataWidth-1 downto 0)
          last := True
        }
        outValid := True
        readyForInput := False

        when (io.out_datastream.fire) {
          burstCntr.increment()
          outValid := False
          when(burstCntr.willOverflowIfInc) {
            internalNonce := internalNonce + 1
            outValid := False
            inReady := True
          }
        }
      }
    }

    is (MACVERIFY){
      when (io.in_datastream.fire) {
        // input data burst
        currData := io.in_datastream.data
//        readyForInput := False

        when (burstCntr.value < 16) {
          when (burstCntr === 3) {
            outTag := B(0, 128 - config.dataWidth bits) ## currData.fragment
          } elsewhen (burstCntr.value % 4 === 3) {
            outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## currData.fragment)
          } otherwise {
            // do nothing
          }

          // when ((burstCntr.value % 2 === 0)) { // odd, sending data
          //   lastData := currData
          // } elsewhen (burstCntr === 1) { // even, first one
          //   outTag := B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment)
          // } elsewhen (burstCntr.value < 8) { // even
          //   outTag := outTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## (lastData.fragment ^ currData.fragment))
          // }

        } elsewhen (burstCntr.value === 20) { // get tag from input stream
          inTag := B(0, 128 - config.dataWidth bits) ## currData.fragment
        } elsewhen (burstCntr.value < 23) {
          inTag := inTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## currData.fragment)
        } elsewhen (burstCntr === 23) {
          inTag := inTag.rotateLeft(32) | (B(0, 128 - config.dataWidth bits) ## currData.fragment)
          data.fragment := currData.fragment    // don't care what you output
          last := True

          when (outTag =/= inTag) {
            error := True
          }
        }  otherwise { // shouldn't go here
          burstCntr.clear()
        }

        when (burstCntr.willOverflowIfInc) {
          outValid := True
          readyForInput := False
        } otherwise {
          burstCntr.increment()
        }

      } elsewhen (io.out_datastream.fire) {
        outValid := False
        inReady := True
        burstCntr.clear()
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
