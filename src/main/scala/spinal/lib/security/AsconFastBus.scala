package spinal.lib.security

import spinal.core._
import spinal.lib.{Counter, Fragment, master, slave}
import spinal.lib.bus.amba4.axi.Axi4Config

//case class AsconFastBusInCmd(ADDR_BUS_WIDTH: Int) extends Bundle {
//  val addr = Bits(ADDR_BUS_WIDTH bits)
//  val wr_en = Bool
//  val burstLen = Bits(2 bits)
//}
//
//case class AsconFastBusInData(DATA_BUS_WIDTH: Int) extends Bundle {
//  val data = Fragment(Bits(DATA_BUS_WIDTH bits))
//}
//
//case class AsconFastBusOutData(DATA_BUS_WIDTH: Int) extends Bundle {
//  val data = Fragment(Bits(DATA_BUS_WIDTH bits))
//}

class AsconFastBus(UNROLLED_ROUNDS: Int = 1,
                   KEY_SIZE: Int = 128,
                   DATA_BLOCK_SIZE: Int = 64,
                   ROUNDS_A: Int = 12,
                   ROUNDS_B: Int = 6,
                   DATA_BUS_WIDTH: Int = 32,
                   ADDR_BUS_WIDTH: Int = 8) extends Component {
  val io = new Bundle {
//    val in_cmdstream = slave Stream(AsconFastBusInCmd(ADDR_BUS_WIDTH))
//    val in_datastream = slave Stream(AsconFastBusInData(DATA_BUS_WIDTH))
//    val out_datastream = master Stream(AsconFastBusOutData(DATA_BUS_WIDTH))
    val valid = in Bool
    val wr_en = in Bool
    val addr = in Bits(ADDR_BUS_WIDTH bits)
    val dataWriteI = in Bits(DATA_BUS_WIDTH bits)
    val dataReadO = out Bits(DATA_BUS_WIDTH bits)
  }

  final val CONTROL_STATE_SIZE = 4
  final val STATE_WORD_SIZE = 64

  final val CONST_UNROLLED_R = B(UNROLLED_ROUNDS, 8 bits)
  final val CONST_KEY_SIZE = B(KEY_SIZE, 8 bits)
  final val CONST_ROUNDS_A = B(ROUNDS_A, 8 bits)
  final val CONST_ROUNDS_B = B(ROUNDS_B, 8 bits)

  val CP_InitxSN = Bool
  val CP_AssociatexSN = Bool
  val CP_EncryptxSN = Bool
  val CP_DecryptxSN = Bool
  val CP_FinalEncryptxSN = Bool
  val CP_FinalDecryptxSN = Bool
  val KeyxDN = Bits(KEY_SIZE bits)

  val CP_InitxSP = RegNext(CP_InitxSN) init(False)
  val CP_AssociatexSP = RegNext(CP_AssociatexSN) init(False)
  val CP_EncryptxSP = RegNext(CP_EncryptxSN) init(False)
  val CP_DecryptxSP = RegNext(CP_DecryptxSN) init(False)
  val CP_FinalEncryptxSP = RegNext(CP_FinalEncryptxSN) init(False)
  val CP_FinalDecryptxSP = RegNext(CP_FinalDecryptxSN) init(False)
  val KeyxDP = RegNext(KeyxDN) init(B(0, KEY_SIZE bits))

  val DP_WriteNoncexS = Bool
  val DP_WriteIODataxS = Bool
  val CP_DonexS = Bool
  val CP_InitxS = Bool
  val CP_AssociatexSI = Bool
  val CP_EncryptxS = Bool
  val CP_DecryptxS = Bool
  val CP_FinalEncryptxS = Bool
  val CP_FinalDecryptxS = Bool

  val IODataxD = Bits(DATA_BLOCK_SIZE bits)
  val StatexD = Vec(Bits(STATE_WORD_SIZE bits), 5)

//  val index = UInt(7 bits)
  val addrReg = RegInit(B(0, ADDR_BUS_WIDTH bits))

  KeyxDN := KeyxDP

  io.dataReadO := B(0, DATA_BUS_WIDTH bits)

  DP_WriteNoncexS := False
  DP_WriteIODataxS := False

  CP_InitxSN := CP_InitxSP
  CP_AssociatexSN := CP_AssociatexSP
  CP_EncryptxSN := CP_EncryptxSP
  CP_DecryptxSN := CP_DecryptxSP
  CP_FinalEncryptxSN := CP_FinalEncryptxSP
  CP_FinalDecryptxSN := CP_FinalDecryptxSP

  when (CP_DonexS) {
    CP_InitxSN := False
    CP_AssociatexSN := False
    CP_EncryptxSN := False
    CP_DecryptxSN := False
    CP_FinalEncryptxSN := False
    CP_FinalDecryptxSN := False
  }

  when (io.valid) {
    when (io.wr_en) {

      when(io.addr === 2) {
        CP_InitxSN := io.dataWriteI(0)
        CP_AssociatexSN := io.dataWriteI(1)
        CP_EncryptxSN := io.dataWriteI(2)
        CP_DecryptxSN := io.dataWriteI(3)
        CP_FinalEncryptxSN := io.dataWriteI(4)
        CP_FinalDecryptxSN := io.dataWriteI(5)
      } elsewhen (io.addr.asUInt >= 4 && io.addr.asUInt < 8) {
        KeyxDN(io.addr(1 downto 0).asUInt * DATA_BUS_WIDTH, DATA_BUS_WIDTH bits) := io.dataWriteI
      } elsewhen (io.addr.asUInt >= 8 && io.addr.asUInt < 12) {
        DP_WriteNoncexS := True
      }
    } otherwise {
      when (io.addr === 0) {
        io.dataReadO := B"32'xDEADBEEF"
      } elsewhen (io.addr === 1) {
        io.dataReadO(0) := CP_InitxSP | CP_AssociatexSP | CP_EncryptxSP | CP_DecryptxSP | CP_FinalEncryptxSP | CP_FinalDecryptxSP
      } elsewhen (io.addr.asUInt >= 12 && io.addr.asUInt < 14) {
        io.dataReadO := IODataxD(io.addr(0).asUInt * DATA_BUS_WIDTH, DATA_BUS_WIDTH bits)
      } elsewhen (io.addr.asUInt >= 16 && io.addr.asUInt < 20) {
        if (DATA_BUS_WIDTH == 64) {
          when (io.addr(1 downto 0) === B"00") {
            io.dataReadO := StatexD(4)
          } elsewhen (io.addr(1 downto 0) === B"00") {
            io.dataReadO := StatexD(3)
          }
        } else {
          io.dataReadO := StatexD(3) ## StatexD(4)
        }
      }
    }
  }

  val ascon = new AsconCore(UNROLLED_ROUNDS = UNROLLED_ROUNDS,
    KEY_SIZE = KEY_SIZE,
    DATA_BLOCK_SIZE = DATA_BLOCK_SIZE,
    ROUNDS_A = ROUNDS_A,
    ROUNDS_B = ROUNDS_B,
    DATA_BUS_WIDTH = DATA_BUS_WIDTH,
    ADDR_BUS_WIDTH = ADDR_BUS_WIDTH)

  ascon.io.AddressxDI := io.addr
  ascon.io.DP_WriteNoncexSI := DP_WriteNoncexS
  ascon.io.DataWritexDI := io.dataWriteI
  ascon.io.KeyxDI := KeyxDP
  ascon.io.DP_WriteIODataxSI := DP_WriteIODataxS
  ascon.io.CP_InitxSI := CP_InitxSP
  ascon.io.CP_AssociatexSI := CP_AssociatexSP
  ascon.io.CP_EncryptxSI := CP_EncryptxSP
  ascon.io.CP_DecryptxSI := CP_DecryptxSP
  ascon.io.CP_FinalEncryptxSI := CP_FinalEncryptxSP
  ascon.io.CP_FinalDecryptxSI := CP_FinalDecryptxSP

  IODataxD := ascon.io.IODataxDO
  CP_DonexS := ascon.io.CP_DonexSO
  StatexD := ascon.io.StatexDO

}
