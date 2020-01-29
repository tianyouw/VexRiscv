package spinal.lib.security

import spinal.core._

/**
  * Created by Jiangyi on 2020-01-09.
  */
class ascon(UNROLED_ROUNDS: Int = 1,
            KEY_SIZE: Int = 128,
            DATA_BLOCK_SIZE: Int = 64,
            ROUNDS_A: Int = 12,
            ROUNDS_B: Int = 6,
            DATA_BUS_WIDTH: Int = 32,
            ADDR_BUS_WIDTH: Int = 8) extends BlackBox {

  // SpinalHDL will look at Generic classes to get attributes which
  // should be used ad VHDL gererics / Verilog parameter
  // You can use String Int Double Boolean and all SpinalHDL base types
  // as generic value
  val generic = new Generic {
//    UNROLED_ROUNDS  : integer := 1; --1,2,3,6
//    KEY_SIZE        : integer := 128;
//    DATA_BLOCK_SIZE : integer := 64;
//    ROUNDS_A        : integer := 12;
//    ROUNDS_B        : integer := 6;
//    DATA_BUS_WIDTH  : integer := 32;
//    ADDR_BUS_WIDTH  : integer := 8);
    val UNROLED_ROUNDS = ascon.this.UNROLED_ROUNDS
    val KEY_SIZE = ascon.this.KEY_SIZE
    val DATA_BLOCK_SIZE = ascon.this.DATA_BLOCK_SIZE
    val ROUNDS_A = ascon.this.ROUNDS_A
    val ROUNDS_B = ascon.this.ROUNDS_B
    val DATA_BUS_WIDTH = ascon.this.DATA_BUS_WIDTH
    val ADDR_BUS_WIDTH = ascon.this.ADDR_BUS_WIDTH
  }
  val io = new Bundle {
//        ClkxCI       : in  std_logic;
//        RstxRBI      : in  std_logic;
//        CSxSI        : in  std_logic;       -- active-high chip select
//        WExSI        : in  std_logic;       -- active-high write enable
//        AddressxDI   : in  std_logic_vector(ADDR_BUS_WIDTH-1 downto 0);
//        DataWritexDI : in  std_logic_vector(DATA_BUS_WIDTH-1 downto 0);
//        DataReadxDO  : out std_logic_vector(DATA_BUS_WIDTH-1 downto 0));
    val ClkxCI = in Bool
    val RstxRBI = in Bool
    val CSxSI = in Bool
    val WExSI = in Bool
    val AddressxDI = in Bits (ADDR_BUS_WIDTH bits)
    val DataWritexDI = in Bits (DATA_BUS_WIDTH bits)
    val DataReadxDO = in Bits (DATA_BUS_WIDTH bits)
  }

//  noIoPrefix()
  mapClockDomain(clock=io.ClkxCI, reset = io.RstxRBI)
  addRTLPath("./rtl/ascon_fast_core.vhdl")
  addRTLPath("./rtl/ascon_fast_bus.vhdl")

}