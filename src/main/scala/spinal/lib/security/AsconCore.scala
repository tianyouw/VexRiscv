//-- Copyright 2014 Graz University of Technology (Hannes Gross <<hannes.gross@iaik.tugraz.at> for original VHDL implementation
//-- Copyright 2020 Andrew Jiang (andrew.jiang@edu.uwaterloo.ca) for SpinalHDL adaptation
//--
//-- Licensed under the Apache License, Version 2.0 (the "License");
//-- you may not use this file except in compliance with the License.
//-- You may obtain a copy of the License at
//  --
//--     http://www.apache.org/licenses/LICENSE-2.0
//--
//-- Unless required by applicable law or agreed to in writing, software
//-- distributed under the License is distributed on an "AS IS" BASIS,
//-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//-- See the License for the specific language governing permissions and
//  -- limitations under the License.
//  -------------------------------------------------------------------------------

package spinal.lib.security

import spinal.core._

class AsconCore(UNROLLED_ROUNDS: Int = 1,
                KEY_SIZE: Int = 128,
                DATA_BLOCK_SIZE: Int = 64,
                ROUNDS_A: Int = 12,
                ROUNDS_B: Int = 6,
                DATA_BUS_WIDTH: Int = 32,
                ADDR_BUS_WIDTH: Int = 8,
                STATE_WORD_SIZE: Int = 64) extends Component {
  val io = new Bundle {
    //    ClkxCI                    : in  std_logic;
    //    RstxRBI                   : in  std_logic;
    //    AddressxDI                : in  std_logic_vector(ADDR_BUS_WIDTH-1 downto 0);
    //    KeyxDI                    : in  std_logic_vector(KEY_SIZE-1 downto 0);
    //    CP_InitxSI                : in  std_logic;
    //    CP_AssociatexSI           : in  std_logic;
    //    CP_EncryptxSI             : in  std_logic;
    //    CP_DecryptxSI             : in  std_logic;
    //    CP_FinalEncryptxSI        : in  std_logic;
    //    CP_FinalDecryptxSI        : in  std_logic;
    //    DP_WriteNoncexSI          : in  std_logic;
    //    DataWritexDI              : in  std_logic_vector(DATA_BUS_WIDTH-1 downto 0);
    //    DP_WriteIODataxSI         : in std_logic;
    //    IODataxDO                 : out std_logic_vector(DATA_BLOCK_SIZE-1 downto 0);
    //    CP_DonexSO                : out std_logic;
    //    StatexDO                  : out std_logic_vector(5*STATE_WORD_SIZE-1 downto 0)

    //    val ClkxCI = in Bool
    //    val RstxRBI = in Bool
//    val AddressxDI = in Bits (ADDR_BUS_WIDTH bits)
    val KeyxDI = in Bits (KEY_SIZE bits)
    val NoncexDI = in Bits (128 bits)
    val CP_InitxSI = in Bool
    val CP_AssociatexSI = in Bool
    val CP_EncryptxSI = in Bool
    val CP_DecryptxSI = in Bool
    val CP_FinalEncryptxSI = in Bool
    val CP_FinalDecryptxSI = in Bool
    val DP_WriteNoncexSI = in Bool
    val DataWritexDI = in Bits (DATA_BUS_WIDTH bits)
//    val DP_WriteIODataxSI = in Bool
    val IODataxDO = out Bits (DATA_BLOCK_SIZE bits)
    val CP_DonexSO = out Bool
    val StatexDO = out Vec(Bits(STATE_WORD_SIZE bits), 5)
  }

//  constant CONTROL_STATE_SIZE : integer := 4;
  final val CONTROL_STATE_SIZE = 4

//  constant CONST_UNROLED_R : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(UNROLED_ROUNDS, 8));
//  constant CONST_KEY_SIZE  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(KEY_SIZE, 8));
//  constant CONST_ROUNDS_A  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_A, 8));
//  constant CONST_ROUNDS_B  : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(ROUNDS_B, 8));
//  constant CONST_RATE      : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(DATA_BLOCK_SIZE, 8));
//
  final val CONST_UNROLLED_R = B(UNROLLED_ROUNDS, 8 bits)
  final val CONST_KEY_SIZE = B(KEY_SIZE, 8 bits)
  final val CONST_ROUNDS_A = B(ROUNDS_A, 8 bits)
  final val CONST_ROUNDS_B = B(ROUNDS_B, 8 bits)
  final val CONST_RATE = B(DATA_BLOCK_SIZE, 8 bits)

//  signal State0xDP, State0xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  signal State1xDP, State1xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  signal State2xDP, State2xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  signal State3xDP, State3xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  signal State4xDP, State4xDN             : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  signal ControlStatexDP, ControlStatexDN : std_logic_vector(CONTROL_STATE_SIZE-1 downto 0);
//
  val StatexDP = Vec(RegInit(B(0, STATE_WORD_SIZE bits)), 5)
  val StatexDN = Vec(Bits(STATE_WORD_SIZE bits), 5)

  for (i <- 0 until 5) {
    StatexDP(i) := StatexDN(i)
  }

  val ControlStatexDN = Bits(CONTROL_STATE_SIZE bits)
  val ControlStatexDP = RegNext(ControlStatexDN) init(B(0, CONTROL_STATE_SIZE bits))

//  signal StatexDP : std_logic_vector(5*STATE_WORD_SIZE-1 downto 0);
  io.CP_DonexSO := CP_DonexS
  io.StatexDO := StatexDP
//  signal DP_InitxS      : std_logic;
//  signal DP_XorZKeyxS   : std_logic;
//  signal DP_XorKeyZxS   : std_logic;
//  signal DP_XorZOnexS   : std_logic;
//  signal DP_EncryptxS   : std_logic;
//  signal DP_DecryptxS   : std_logic;
//  signal DP_AssociatexS : std_logic;
//  signal CP_DonexS      : std_logic;
//  signal DP_RoundxSN, DP_RoundxSP                             : std_logic;
//  signal CP_FinalAssociatedDataxSN, CP_FinalAssociatedDataxSP : std_logic;

  val DP_InitxS = Bool
  val DP_XorZKeyxS = Bool
  val DP_XorKeyZxS = Bool
  val DP_XorZOnexS = Bool
  val DP_EncryptxS = Bool
  val DP_DecryptxS = Bool
  val DP_AssociatexS = Bool
  val CP_DonexS = Bool

  val DP_RoundxSN = Bool
  val DP_RoundxSP = RegNext(DP_RoundxSN) init(False)

  val CP_FinalAssociatedDataxSN = Bool
  val CP_FinalAssociatedDataxSP = RegNext(CP_FinalAssociatedDataxSN) init(False)


  // BEGIN ControlProc
  CP_FinalAssociatedDataxSN := CP_FinalAssociatedDataxSP

  DP_InitxS := False
  DP_XorZKeyxS := False
  DP_XorKeyZxS := False
  DP_XorZOnexS := False
  DP_EncryptxS := False
  DP_DecryptxS := False
  DP_AssociatexS := False
  CP_DonexS := False

  ControlStatexDN := ControlStatexDP

  when (ControlStatexDP === 0) {
    DP_InitxS := io.CP_InitxSI
    DP_AssociatexS := io.CP_AssociatexSI
    DP_EncryptxS := io.CP_EncryptxSI | io.CP_FinalEncryptxSI
    DP_DecryptxS := io.CP_DecryptxSI | io.CP_FinalDecryptxSI
    DP_XorKeyZxS := io.CP_FinalEncryptxSI | io.CP_FinalDecryptxSI

    when (io.CP_InitxSI) {
      CP_FinalAssociatedDataxSN := False
    }

    when (io.CP_EncryptxSI || io.CP_DecryptxSI || io.CP_FinalEncryptxSI || io.CP_FinalDecryptxSI) {
      CP_FinalAssociatedDataxSN := False
      when (!CP_FinalAssociatedDataxSP) {
        DP_XorZOnexS := True
      }
    }
  }

  when (io.CP_InitxSI || io.CP_AssociatexSI || io.CP_EncryptxSI || io.CP_DecryptxSI || io.CP_FinalEncryptxSI || io.CP_FinalDecryptxSI) {
    ControlStatexDN := B(ControlStatexDP.asUInt + UNROLLED_ROUNDS, CONTROL_STATE_SIZE bits)
    DP_RoundxSN := True
  }

  when ((io.CP_InitxSI || io.CP_FinalEncryptxSI || io.CP_FinalDecryptxSI) && ControlStatexDP === ROUNDS_A - UNROLLED_ROUNDS) {
    ControlStatexDN := 0
    DP_XorZKeyxS := True
    CP_DonexS := True
  }

  when ((io.CP_AssociatexSI || io.CP_EncryptxSI || io.CP_DecryptxSI) && ControlStatexDP === ROUNDS_B - UNROLLED_ROUNDS) {
    ControlStatexDN := 0
    CP_DonexS := True
  }

  // END ControlProc

  // BEGIN DatapathProc
//  variable P0xDV, P1xDV, P2xDV, P3xDV, P4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  variable R0xDV, R1xDV, R2xDV, R3xDV, R4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  variable S0xDV, S1xDV, S2xDV, S3xDV, S4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  variable T0xDV, T1xDV, T2xDV, T3xDV, T4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  variable U0xDV, U1xDV, U2xDV, U3xDV, U4xDV : std_logic_vector(STATE_WORD_SIZE-1 downto 0);
//  variable RoundConstxDV                     : std_logic_vector(63 downto 0);
//  variable State0XorIODataxDV                : std_logic_vector(63 downto 0);
//  variable State1XorIODataxDV                : std_logic_vector(63 downto 0);
  val PxDV = Vec(Bits(STATE_WORD_SIZE bits), 5)
  val RxDV = Vec(Bits(STATE_WORD_SIZE bits), 5)
  val SxDV = Vec(Bits(STATE_WORD_SIZE bits), 5)
  val TxDV = Vec(Bits(STATE_WORD_SIZE bits), 5)
  val UxDV = Vec(Bits(STATE_WORD_SIZE bits), 5)
  val RoundConstxDV = Bits(64 bits)
  val State0XorIODataxDV = Bits(64 bits)
  val State1XorIODataxDV = Bits(64 bits)

  for (i <- 0 until 5) {
    StatexDN(i) = StatexDP(i)
    PxDV(i) := StatexDP(i)
  }

  when (DP_InitxS) {
    PxDV(0) := CONST_KEY_SIZE ## CONST_RATE ## CONST_ROUNDS_A ## CONST_ROUNDS_B ## B(0, 32 bits)
    PxDV(1) := io.KeyxDI(127 downto 64)
    PxDV(2) := io.KeyxDI(63 downto 0)
    StatexDN(3) := io.NoncexDI(127 downto 64)
    StatexDN(4) := io.NoncexDI(63 downto 0)
  }

  // For 128 variant
  if (DATA_BLOCK_SIZE == 64) {
    State0XorIODataxDV := StatexDP(0) ^ io.DataWritexDI(63 downto 0)
    io.IODataxDO := State0XorIODataxDV
    // Finalization
    when (DP_XorZKeyxS) {
      PxDV(1) := StatexDP(1) ^ io.KeyxDI(127 downto 64)
      PxDV(2) := StatexDP(2) ^ io.KeyxDI(63 downto 0)
    }

    when (DP_EncryptxS || DP_AssociatexS) {
      PxDV(0) := State0XorIODataxDV
    }
  // For 128a variant
  } else if (DATA_BLOCK_SIZE == 128) {
    State0XorIODataxDV := StatexDP(0) ^ io.DataWritexDI(127 downto 64)
    State1XorIODataxDV := StatexDP(1) ^ io.DataWritexDI(63 downto 0)
    io.IODataxDO := State0XorIODataxDV ## State1XorIODataxDV

    // Finalization
    when (DP_XorZKeyxS) {
      PxDV(2) := StatexDP(2) ^ io.KeyxDI(127 downto 64)
      PxDV(3) := StatexDP(3) ^ io.KeyxDI(63 downto 0)
    }

    when (DP_EncryptxS || DP_AssociatexS) {
      PxDV(0) := State0XorIODataxDV
      PxDV(1) := State1XorIODataxDV
    }
  }

  PxDV(4) := PxDV(0) ^ DP_XorZOnexS.asBits(1 bit)

  when (DP_DecryptxS) {
    PxDV(0) := io.DataWritexDI
  }

  for (r <- 0 until UNROLLED_ROUNDS) {
    RoundConstxDV := B(0, 64 - 8 bits) ## ~B(ControlStatexDP(3 downto 0).asUInt + r, CONTROL_STATE_SIZE bits) ## B(ControlStatexDP(3 downto 0).asUInt + r, CONTROL_STATE_SIZE bits)

    RxDV(0) := PxDV(0) ^ PxDV(4)
    RxDV(1) := PxDV(1)
    RxDV(2) := PxDV(2) ^ PxDV(1) ^ RoundConstxDV
    RxDV(3) := PxDV(3)
    RxDV(4) := PxDV(4) ^ PxDV(3)

    SxDV(0) := RxDV(0) ^ (~RxDV(1) & RxDV(2))
    SxDV(1) := RxDV(1) ^ (~RxDV(2) & RxDV(3))
    SxDV(2) := RxDV(2) ^ (~RxDV(3) & RxDV(4))
    SxDV(3) := RxDV(3) ^ (~RxDV(4) & RxDV(0))
    SxDV(4) := RxDV(4) ^ (~RxDV(0) & RxDV(1))

    TxDV(0) := SxDV(0) ^ SxDV(4)
    TxDV(1) := SxDV(1) ^ SxDV(0)
    TxDV(2) := ~SxDV(2)
    TxDV(3) := SxDV(3) ^ SxDV(2)
    TxDV(4) := SxDV(4)

    UxDV(0) := TxDV(0) ^ TxDV(0).rotateRight(19) ^ TxDV(0).rotateRight(28)
    UxDV(1) := TxDV(1) ^ TxDV(1).rotateRight(61) ^ TxDV(1).rotateRight(39)
    UxDV(2) := TxDV(2) ^ TxDV(2).rotateRight(1) ^ TxDV(2).rotateRight(6)
    UxDV(3) := TxDV(3) ^ TxDV(3).rotateRight(10) ^ TxDV(3).rotateRight(17)
    UxDV(4) := TxDV(4) ^ TxDV(4).rotateRight(7) ^ TxDV(4).rotateRight(41)

    PxDV(0) := UxDV(0)
    PxDV(1) := UxDV(1)
    PxDV(2) := UxDV(2)
    PxDV(3) := UxDV(3)
    PxDV(4) := UxDV(4)
  }

  when (DP_XorZKeyxS) {
    UxDV(3) := UxDV(3) ^ io.KeyxDI(127 downto 64)
    UxDV(4) := UxDV(4) ^ io.KeyxDI(63 downto 0)
  }

  when (DP_RoundxSN) {
    for (i <- 0 until 5) {
      StatexDN(i) := UxDV(i)
    }
  }

//  when (io.DP_WriteNoncexSI) {
//    if (DATA_BUS_WIDTH == 32) {
//      when (io.AddressxDI(1 downto 0) === B"2b00") {
//        StatexDN(4)(31 downto 0) := io.DataWritexDI
//      } elsewhen(io.AddressxDI(1 downto 0) === B"2b01") {
//        StatexDN(4)(63 downto 32) := io.DataWritexDI
//      } elsewhen(io.AddressxDI(1 downto 0) === B"2b10") {
//        StatexDN(3)(31 downto 0) := io.DataWritexDI
//      } otherwise {
//        StatexDN(3)(63 downto 32) := io.DataWritexDI
//      }
//    } else if (DATA_BUS_WIDTH == 64) {
//      when (!io.AddressxDI(0)) {
//        StatexDN(4) := io.DataWritexDI
//      } otherwise {
//        StatexDN(3) := io.DataWritexDI
//      }
//    } else if (DATA_BUS_WIDTH == 128) {
//      when (!io.AddressxDI(0)) {
//        StatexDN(4) := io.DataWritexDI(63 downto 0)
//        StatexDN(3) := io.DataWritexDI(127 downto 64)
//      }
//    } else {
//      // TODO: Implement for 16-bit and 8-bit bus width
//    }
//  }
}
