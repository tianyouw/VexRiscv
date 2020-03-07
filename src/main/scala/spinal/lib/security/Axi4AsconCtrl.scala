package spinal.lib.security

import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.{Counter, slave}

/**
  * Created by Jiangyi on 2020-03-06.
  */
object Axi4AsconCtrl {
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



case class Axi4AsconCtrl(axiDataWidth: Int, axiAddrWidth: Int, axiIdWidth: Int) extends Component {
  val axiConfig = Axi4AsconCtrl.getAxiConfig(axiDataWidth, axiAddrWidth, axiIdWidth)
  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
  }

  def resetAsconCmd(): Unit = {
    asconInitReg := False
    asconAssociateReg := False
    asconEncryptReg := False
    asconDecryptReg := False
    asconFinalEncryptReg := False
    asconFinalDecryptReg := False
  }

  val asconCore = new AsconCore(DATA_BLOCK_SIZE = 128, DATA_BUS_WIDTH = 128, ROUNDS_B = 8, UNROLLED_ROUNDS = 4)
  val keyIn = Reg(Bits(128 bits))
  val dataIn = Reg(Bits(128 bits))
  val nonceIn = Reg(Bits(128 bits))
  val dataOut = Reg(Bits(128 bits))
  val tagOut = Bits(128 bits)

  val asconInitReg = RegInit(False)
  val asconAssociateReg = RegInit(False)
  val asconEncryptReg = RegInit(False)
  val asconDecryptReg = RegInit(False)
  val asconFinalEncryptReg = RegInit(False)
  val asconFinalDecryptReg = RegInit(False)

  asconCore.io.KeyxDI := keyIn
  asconCore.io.NoncexDI := nonceIn
  asconCore.io.DataWritexDI := dataIn
  asconCore.io.CP_InitxSI := asconInitReg
  asconCore.io.CP_AssociatexSI := asconAssociateReg
  asconCore.io.CP_EncryptxSI := asconEncryptReg
  asconCore.io.CP_DecryptxSI := asconDecryptReg
  asconCore.io.CP_FinalEncryptxSI := asconFinalEncryptReg
  asconCore.io.CP_FinalDecryptxSI := asconFinalDecryptReg

  tagOut := asconCore.io.StatexDO(3) ## asconCore.io.StatexDO(4)

  val busyReg = RegInit(False)
  val addrReg = Reg(UInt(5 bits))
  val isWriteReg = Reg(Bool)
  val writeDoneReg = RegInit(False)
  val idReg = Reg(UInt(axiConfig.idWidth bits))
  val asconCoreDoneReg = RegInit(False)

  io.axi.sharedCmd.ready := !busyReg
  io.axi.writeData.ready := io.axi.sharedCmd.fire || busyReg

  io.axi.readRsp.valid := False
  io.axi.readRsp.last := busyReg && !isWriteReg // Only send 1 word
  io.axi.readRsp.setOKAY()
  io.axi.readRsp.data.assignDontCare()
  io.axi.readRsp.id := idReg

  io.axi.writeRsp.setOKAY()
  io.axi.writeRsp.valid := writeDoneReg
  io.axi.writeRsp.id := idReg

  when (asconCore.io.CP_DonexSO) {
    resetAsconCmd()
    asconCoreDoneReg := True
  }

  when (io.axi.sharedCmd.fire) {
//    assert(io.axi.sharedCmd.len === 1)
    addrReg := io.axi.sharedCmd.addr(6 downto 2)
    isWriteReg := io.axi.sharedCmd.write
    idReg := io.axi.sharedCmd.id
    busyReg := True

  }

  when (busyReg && !isWriteReg) {
    io.axi.readRsp.valid := True
    when (addrReg === 0) {
      io.axi.readRsp.data := B"32'xDEADBEEF"
    } elsewhen (addrReg === 1) {
      io.axi.readRsp.data(0) := asconInitReg
      io.axi.readRsp.data(1) := asconAssociateReg
      io.axi.readRsp.data(2) := asconEncryptReg
      io.axi.readRsp.data(3) := asconDecryptReg
      io.axi.readRsp.data(4) := asconFinalEncryptReg
      io.axi.readRsp.data(5) := asconFinalDecryptReg
    } elsewhen (addrReg >= 12 && addrReg < 16) {
      io.axi.readRsp.data := dataOut(addrReg(1 downto 0) * axiConfig.dataWidth, axiConfig.dataWidth bits)
    } elsewhen (addrReg >= 16 && addrReg < 20) {
      io.axi.readRsp.data := tagOut(addrReg(1 downto 0) * axiConfig.dataWidth, axiConfig.dataWidth bits)
    }
  }

  when (io.axi.writeData.fire) {
    val addr = UInt(5 bits)
    val wr_en = Bool
    val id = UInt(axiConfig.idWidth bits)
    when (io.axi.sharedCmd.fire) {
      addr := io.axi.sharedCmd.addr(6 downto 2)
      wr_en := io.axi.sharedCmd.write
      id := io.axi.sharedCmd.id
    } otherwise {
      addr := addrReg
      wr_en := isWriteReg
      id := idReg
    }
//    assert(wr_en === True)
    when (addr === 2) {
      asconInitReg := io.axi.writeData.data(0)
      asconAssociateReg := io.axi.writeData.data(1)
      asconEncryptReg := io.axi.writeData.data(2)
      asconDecryptReg := io.axi.writeData.data(3)
      asconFinalEncryptReg := io.axi.writeData.data(4)
      asconFinalDecryptReg := io.axi.writeData.data(5)

      when (io.axi.writeData.data(5 downto 2).orR) {
        dataOut := asconCore.io.IODataxDO
      }
    } elsewhen (addr >= 4 && addr < 8) {
      keyIn(addr(1 downto 0) * axiConfig.dataWidth, axiConfig.dataWidth bits) := io.axi.writeData.data
    } elsewhen (addr >= 8 && addr < 12) {
      nonceIn(addr(1 downto 0) * axiConfig.dataWidth, axiConfig.dataWidth bits) := io.axi.writeData.data
    } elsewhen (addr >= 12 && addr < 16) {
      dataIn(addr(1 downto 0) * axiConfig.dataWidth, axiConfig.dataWidth bits) := io.axi.writeData.data
    }
//    assert(io.axi.writeData.last)
    writeDoneReg := True
  }

  when ((isWriteReg && io.axi.writeRsp.fire) || (!isWriteReg && io.axi.readRsp.fire && io.axi.readRsp.last)) {
    busyReg := False
    asconCoreDoneReg := False
    writeDoneReg := False
  }
}
