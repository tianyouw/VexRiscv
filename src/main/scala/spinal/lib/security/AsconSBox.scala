package spinal.lib.security

import spinal.core._

class AsconSBox(STATE_WORD_SIZE : Int = 64,
                CONTROL_STATE_SIZE : Int = 4) extends Component {

  val io = new Bundle {
    val PxDV = in Vec(Bits(STATE_WORD_SIZE bits), 5)
    val round = in UInt(4 bits) // We'll go up to 12 at most; 4 bits will suffice
    val controlState = in Bits(CONTROL_STATE_SIZE bits)
    val UxDV = out Vec(Bits(STATE_WORD_SIZE bits), 5)
  }

  assert(io.round <= 12)
  val RxDV = Vec(Bits(STATE_WORD_SIZE bits), 5)
  val SxDV = Vec(Bits(STATE_WORD_SIZE bits), 5)
  val TxDV = Vec(Bits(STATE_WORD_SIZE bits), 5)
  var RoundConstxDV = B(0, 64 - 8 bits) ## ~B(io.controlState(3 downto 0).asUInt + io.round, CONTROL_STATE_SIZE bits) ## B(io.controlState(3 downto 0).asUInt + io.round, CONTROL_STATE_SIZE bits)

  RxDV(0) := io.PxDV(0) ^ io.PxDV(4)
  RxDV(1) := io.PxDV(1)
  RxDV(2) := io.PxDV(2) ^ io.PxDV(1) ^ RoundConstxDV
  RxDV(3) := io.PxDV(3)
  RxDV(4) := io.PxDV(4) ^ io.PxDV(3)

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

  io.UxDV(0) := TxDV(0) ^ TxDV(0).rotateRight(19) ^ TxDV(0).rotateRight(28)
  io.UxDV(1) := TxDV(1) ^ TxDV(1).rotateRight(61) ^ TxDV(1).rotateRight(39)
  io.UxDV(2) := TxDV(2) ^ TxDV(2).rotateRight(1) ^ TxDV(2).rotateRight(6)
  io.UxDV(3) := TxDV(3) ^ TxDV(3).rotateRight(10) ^ TxDV(3).rotateRight(17)
  io.UxDV(4) := TxDV(4) ^ TxDV(4).rotateRight(7) ^ TxDV(4).rotateRight(41)

}
