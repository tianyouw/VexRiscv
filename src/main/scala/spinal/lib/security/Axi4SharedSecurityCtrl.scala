package spinal.lib.security

import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4Arw, Axi4Config, Axi4Shared}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.memory.sdram.{Axi4SharedSdramCtrl, SdramLayout}
import spinal.lib.{Counter, CounterUpDown, Fragment, Stream, StreamFifo, master, slave}

/**
  * Created by Jiangyi on 2019-07-14.
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
  val axiConfig = Axi4SharedSdramCtrl.getAxiConfig(axiDataWidth, axiIdWidth, layout)
  val writeToRam = Reg(Bool())
  val error = Bool()

  final val PASSTHROUGH = false

  // -------------Tree calculation stuff end
  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    val sdramAxi = master(Axi4Shared(axiConfig))
  }

  // -------------Tree calculation stuff start
  final val treeAry = 4
  final val treeStart = 0x1000000 // TODO: andrew, replace this with whatever you want
  final val directAccessStart = 0x4000000
  final val memorySizeBytes = 16 * 1024 * 1024 // TODO: andrew, get this from somewhere else?
  final val blockSizeBytes = 32 //24

//  def getParent(block: Int) = (block - 1) / treeAry
  def getChild(block: Int, childNum: Int) = treeAry * block + childNum + 1
  def getNumLayers(memorySize: Int): Int = {
    var prevLayerBlock = 0
    var currentLayerBlock = 1
    var layerCounter = 1
    while ((currentLayerBlock - prevLayerBlock) < memorySize / 32) {
      var next = getChild(currentLayerBlock, 0)
      prevLayerBlock = currentLayerBlock
      currentLayerBlock = next
      layerCounter += 1
    }

    layerCounter
  }


  def IDLE_STATE: UInt = 0
  def VERIFY_TAG_FROM_SDRAM_STATE: UInt = 1
  def DECRYPT_DATA_STATE: UInt = 2
  def ENCRYPT_DATA_STATE : UInt = 3
  def WRITE_DATA_STATE : UInt = 4
  def WRITE_NEW_TAG_STATE : UInt = 5
  def CALCULATE_NEW_TAG_STATE : UInt = 6
  def RETURN_DATA_STATE : UInt = 7

//  if (PASSTHROUGH) {
//    io.axi <> io.sdramAxi
//  } else {
    val numLayers = getNumLayers(memorySizeBytes)

    val layerAddressVec = Vec(UInt(axiConfig.addressWidth bits), numLayers)

    var blockNum = 0
    for (layer <- 0 until numLayers) {
      layerAddressVec(layer) := treeStart + blockNum * blockSizeBytes
      blockNum = getChild(blockNum, 0)
    }

  val layerIndexReg = RegInit(U(numLayers))
    // Data = 8 bytes, tag = 4 bytes, nonce = 4 bytes
//    val dataInFifo = StreamFifo(dataType = CAESARCtrlInData(axiConfig), depth = 16)
//    val dataOutFifo = StreamFifo(dataType = CAESARCtrlOutData(axiConfig), depth = 16)
    // Data = 8 bytes, nonce = 4 bytes
    val dataInFifo = StreamFifo(dataType = AsconCtrlInData(axiConfig), depth = 12)
    // Data = 8 bytes, tag = 4 bytes, nonce = 4 bytes
    val dataOutFifo = StreamFifo(dataType = AsconCtrlOutData(axiConfig), depth = 16)

    // FIFO for setting aside decrypted data to send back to the CPU
    val dataSetAsideFifo = StreamFifo(dataType = Fragment(Bits(axiDataWidth bits)), depth = 8)
    val nextNonceTagBlockFifo = StreamFifo(dataType = Fragment(Bits(axiConfig.dataWidth bits)), depth = 8)

    // Tree address registers
    //  val currentNodeFirstSiblingStartAddrOffsetReg = Reg(UInt(axiConfig.addressWidth bits))
    val currentAddrOffsetReg = Reg(UInt(axiConfig.addressWidth bits))


    val currentLevelStartAddr = UInt(axiConfig.addressWidth bits)
    currentLevelStartAddr := layerAddressVec(layerIndexReg)

    val nonceVecReg = Vec(Reg(Bits(axiConfig.dataWidth bits)), 16)

    val tagSetAsideFifo = StreamFifo(dataType = Bits(axiDataWidth bits), depth = 4)
    val tagVerifiedReg = RegInit(True)

    val decryptVerifyCounter = Counter(0 until 24)
    val pendingWordsCounter = Counter(0 until 8)
    val pendingNonceCounter = Counter(0 until 16)

    val tagPartsVerifiedCounter = Counter(0 until 4)

//    val caesarCtrl = DummyCAESARCtrl(axiConfig)
    val asconFastCtrl = new AsconFastCtrl(axiConfig)
    //  val sharedCmdReg = RegNextWhen(io.axi.sharedCmd, io.axi.sharedCmd.fire)

    val sdramWrEnReg = RegInit(False)
    val writeRspValidReg = RegInit(False)
    val sdramAxiSharedCmdValidReg = RegInit(False)


    val axiSharedCmdReg = Reg(Stream(Axi4Arw(axiConfig)))
    val dataReg = Reg(Bits(axiDataWidth bits))
    val strbReg = Reg(Bits(axiConfig.bytePerWord bits))
    val busyReg = RegInit(False)
    val asconInputCmdValidReg = RegInit(False)


    def bytePosition(addr: UInt): UInt = addr(4 downto 2)

    def combineNewDataWithOriginal(originalData: Bits, newData: Bits, strbMask: Bits): Bits = {
      assert(originalData.getWidth == newData.getWidth)
      assert(newData.getWidth == 32)
      val outData = newData

      when(strbMask(3)) {
        outData(31 downto 24) := newData(31 downto 24)
      }

      when(strbMask(2)) {
        outData(23 downto 16) := newData(23 downto 16)
      }

      when(strbMask(1)) {
        outData(15 downto 8) := newData(15 downto 8)
      }

      when(strbMask(0)) {
        outData(7 downto 0) := newData(7 downto 0)
      }

      outData
    }

    def getSdramDataAddress(originalAddr: UInt): UInt = {
      originalAddr(axiConfig.addressWidth - 1 downto 5) @@ U"00000"
    }

    //  // Assuming 4-ary for now
    //  def getTreeLeafNodeTagAddress(originalAddr: UInt): UInt = {
    //    0x4000000 + (originalAddr(axiConfig.addressWidth - 1 downto 5).resize(axiConfig.addressWidth) * 12)
    //  }
    //
    def updateToNextParentNodeAddrReg(): Unit = {
      layerIndexReg := layerIndexReg - 1
      currentAddrOffsetReg := getParentNodeAddrOffset()
    }

    //
    def getCurrentTagNodeAddr(): UInt = {
      currentLevelStartAddr + currentAddrOffsetReg
    }

    //
    def getCurrentTagNodeFirstSiblingAddr(): UInt = {
      (currentLevelStartAddr + getFirstSiblingAddrOffset(currentAddrOffsetReg)).resize(axiConfig.addressWidth)
    }

    //
    def getFirstSiblingAddrOffset(addr: UInt): UInt = {
      addr(axiConfig.addressWidth - 1 downto 7) @@ U(0, 7 bits)
    }
    //
    //
    //  def getParentNodeAddrOffset() : UInt = ((((currentAddrOffsetReg >> 5) - 1) >> 2) << 5).resize(axiConfig.addressWidth)

    def getParentNodeAddrOffset(): UInt = (currentAddrOffsetReg(axiConfig.addressWidth - 1 downto 7) @@ U(0, 5 bits)).resize(axiConfig.addressWidth)

    def getParentNodeAddr(): UInt = (layerAddressVec(layerIndexReg - 1) + getParentNodeAddrOffset()).resize(axiConfig.addressWidth)

    //
    def isRootOfTree(): Bool = layerIndexReg === 0

    def isParentRootOfTree(): Bool = layerIndexReg === 1

    def getSiblingIndex(): UInt = {
      currentAddrOffsetReg(6 downto 5)
      //    val blockNum = (currentAddrOffsetReg >> 5)
      //    val antimask = U(0x03, 22 bits)
      //    (blockNum - ((blockNum - 1) & ~antimask) - 1).resize(2 bits)
    }

    dataInFifo.io.push.valid := False
    dataInFifo.io.push.payload.assignDontCare()

    asconFastCtrl.io.out_datastream <> dataOutFifo.io.push
    asconFastCtrl.io.in_datastream <> dataInFifo.io.pop
    asconFastCtrl.io.in_cmdstream.payload.assignDontCare()
    //  caesarCtrl.io.in_cmdstream.valid := False

    //  dataOutFifo.io.pop.ready := False

    dataSetAsideFifo.io.push.valid := False
    dataSetAsideFifo.io.push.payload.assignDontCare()
    dataSetAsideFifo.io.pop.ready := False

    tagSetAsideFifo.io.push.valid := False
    tagSetAsideFifo.io.push.payload.assignDontCare()
    tagSetAsideFifo.io.pop.ready := False

    nextNonceTagBlockFifo.io.push.valid := False
    nextNonceTagBlockFifo.io.push.payload.assignDontCare()
    nextNonceTagBlockFifo.io.pop.ready := False

    val writeDataCompleteReg = RegInit(False)

    val decryptStateReadParentNodeReg = RegInit(False)
    val decryptStateDoneDataDecryptReg = RegInit(False)

    val writeDataStateDoneStoringNonceReg = RegInit(False)
    val calculateNewTagStateDoneReceivingNonceReg = RegInit(False)

    val calculateNewTagStateDataInFifoValidReg = RegInit(True)

    val verifyTagStateReadCompleteReg = RegInit(False)

    val tempNonceReg = Vec(Reg(Bits(axiConfig.dataWidth bits)), 4)

    //  when (io.axi.sharedCmd.addr >= treeStart && !busyReg) {
    //    // Accessing tree section; bypass and let it through directly
    //    io.axi <> io.sdramAxi
    //  } otherwise {
    //    (busyReg || (io.axi.sharedCmd.addr < treeStart && io.axi.sharedCmd.valid)) {
    dataOutFifo.io.pop.ready := io.sdramAxi.writeData.ready


    io.sdramAxi.writeData.valid := dataOutFifo.io.pop.valid && sdramWrEnReg
    io.sdramAxi.writeData.data := dataOutFifo.io.pop.payload.data.fragment
    io.sdramAxi.writeData.strb := "1111"
    io.sdramAxi.writeData.last := dataOutFifo.io.pop.payload.data.last

    io.sdramAxi.sharedCmd.size := io.axi.sharedCmd.size
    //    when (io.sdramAxi.sharedCmd.write) {
    //      io.sdramAxi.sharedCmd.len  := 0
    //      io.sdramAxi.sharedCmd.addr := axiSharedCmdReg.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
    //
    //    } otherwise {
    when(axiSharedCmdReg.write) {
      io.sdramAxi.sharedCmd.len := 7
    } otherwise {
      io.sdramAxi.sharedCmd.len := axiSharedCmdReg.len
    }
    //      io.sdramAxi.sharedCmd.id := 8
    //    }
    io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
    io.sdramAxi.sharedCmd.id := axiSharedCmdReg.id
    io.sdramAxi.sharedCmd.burst := axiSharedCmdReg.burst
    io.sdramAxi.sharedCmd.write := sdramWrEnReg
    //    io.sdramAxi.sharedCmd.addr := io.axi.sharedCmd.addr
    io.sdramAxi.sharedCmd.valid := sdramAxiSharedCmdValidReg
    io.sdramAxi.writeRsp.ready := True


    io.axi.sharedCmd.ready := io.axi.sharedCmd.valid && !busyReg
    io.axi.writeData.ready := io.axi.writeData.valid && !busyReg
    io.axi.writeRsp.valid := writeRspValidReg
    io.axi.writeRsp.resp := io.sdramAxi.writeRsp.resp
    io.axi.writeRsp.payload.id := io.sdramAxi.writeRsp.payload.id

    io.axi.readRsp.valid := False
    io.axi.readRsp.data := io.sdramAxi.readRsp.data
    io.axi.readRsp.last := io.sdramAxi.readRsp.last
    io.axi.readRsp.id := io.sdramAxi.readRsp.id
    io.axi.readRsp.resp := io.sdramAxi.readRsp.resp

    io.sdramAxi.readRsp.ready := dataInFifo.io.push.ready

    asconFastCtrl.io.in_cmdstream.valid := asconInputCmdValidReg

    when(asconFastCtrl.io.in_cmdstream.fire) {
      asconInputCmdValidReg := False
    }

    val bypassIsWriteReg = RegInit(False)
    val fsm = new StateMachine {
      val bypassState: State = new State {
        whenIsActive {
          io.axi <> io.sdramAxi
          io.sdramAxi.sharedCmd.valid.allowOverride
          io.sdramAxi.sharedCmd.valid := False
          when (io.axi.sharedCmd.addr >= directAccessStart) {
            io.sdramAxi.sharedCmd.addr := io.axi.sharedCmd.addr - directAccessStart
          }
          when ((!bypassIsWriteReg && io.axi.readRsp.fire && io.axi.readRsp.last) || (bypassIsWriteReg && io.axi.writeRsp.fire)) {
            goto(idleState)
          }
        }
      }

      val idleState: State = new State with EntryPoint {
        onEntry {
          pendingWordsCounter := 0
          sdramWrEnReg := False
        }

        whenIsActive {
          when(busyReg || (io.axi.sharedCmd.valid && io.axi.sharedCmd.addr < treeStart)) {
            when(io.axi.sharedCmd.fire) {
              busyReg := True
              axiSharedCmdReg := io.axi.sharedCmd
              writeDataCompleteReg := False
              decryptStateReadParentNodeReg := False
              writeDataStateDoneStoringNonceReg := False
              calculateNewTagStateDataInFifoValidReg := True
              verifyTagStateReadCompleteReg := False

              layerIndexReg := numLayers - 1
              //            currentLevelStartAddrReg := 0x4000000
              currentAddrOffsetReg := io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000"
              //            currentNodeFirstSiblingStartAddrOffsetReg := getFirstSiblingAddrOffset(io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) * 0xC).resize(axiConfig.addressWidth)

              // For the read case, go directly to read state
              when(!io.axi.sharedCmd.write) {
                asconInputCmdValidReg := True
                sdramAxiSharedCmdValidReg := True
                goto(verifyTagFromSdramState)
              }
            }

            // These two blocks are for the write case
            when(io.axi.writeData.fire) {
              writeRspValidReg := True
              dataReg := io.axi.writeData.data
              strbReg := io.axi.writeData.strb
              io.axi.writeRsp.isOKAY()
            }

            when(io.axi.writeRsp.fire) {
              writeRspValidReg := False
              sdramAxiSharedCmdValidReg := True
              asconInputCmdValidReg := True
              goto(verifyTagFromSdramState)
            }
          } elsewhen(io.axi.sharedCmd.valid && io.axi.sharedCmd.addr >= treeStart && io.axi.sharedCmd.addr < directAccessStart) {
            io.axi <> io.sdramAxi
            bypassIsWriteReg := io.axi.sharedCmd.write
            when (io.axi.sharedCmd.fire) {
              goto(bypassState)
            }
          } elsewhen(io.axi.sharedCmd.valid && io.axi.sharedCmd.addr >= directAccessStart) {
            io.axi <> io.sdramAxi
            bypassIsWriteReg := io.axi.sharedCmd.write
            io.sdramAxi.sharedCmd.addr.allowOverride
            //            io.axi.writeData <> io.sdramAxi.writeData
//            io.axi.writeRsp <> io.sdramAxi.writeRsp
//            io.axi.readRsp <> io.sdramAxi.readRsp
            io.sdramAxi.sharedCmd.addr := io.axi.sharedCmd.addr - directAccessStart
            when (io.axi.sharedCmd.fire) {
              goto(bypassState)
            }
//            goto(bypassOffsetState)
//            io.sdramAxi.sharedCmd.size := io.axi.sharedCmd.size
//            io.sdramAxi.sharedCmd.len := io.axi.sharedCmd.len
//            io.sdramAxi.sharedCmd.id := io.axi.sharedCmd.id
//            io.sdramAxi.sharedCmd.burst := io.axi.sharedCmd.burst
//            io.sdramAxi.sharedCmd.write := io.axi.sharedCmd.write
//            io.sdramAxi.sharedCmd.valid := io.axi.sharedCmd.valid
//            io.axi.sharedCmd.ready := io.sdramAxi.sharedCmd.ready
          }
        }
      }

      val verifyTagFromSdramState: State = new State {
        onEntry {
          verifyTagStateReadCompleteReg := False
        }
        whenIsActive {
          //          when(isRootOfTree()) {
          //            when (axiSharedCmdReg.write) {
          //              sdramAxiSharedCmdValidReg := True
          //              sdramWrEnReg := True
          //              goto(writeNewTagState)
          //            } otherwise {
          //              goto(decryptDataState)
          //            }
          //          }

          io.sdramAxi.writeData.valid := False
          dataOutFifo.io.push.valid := False
          asconFastCtrl.io.in_cmdstream.mode := asconFastCtrl.MACVER
//          asconFastCtrl.io.out_datastream.ready := verifyTagStateReadCompleteReg
          asconFastCtrl.io.out_datastream.ready := tagSetAsideFifo.io.pop.valid

          when(decryptVerifyCounter.value < 8) {
            io.sdramAxi.sharedCmd.addr := getParentNodeAddr()
            io.sdramAxi.sharedCmd.len := 7 // Burst of 8, since 128 bit nonce + 128 bit tag
          } otherwise {
            io.sdramAxi.sharedCmd.addr := getCurrentTagNodeFirstSiblingAddr() + ((decryptVerifyCounter - 8) << 3)
            io.sdramAxi.sharedCmd.len := 3 // Burst of 4, since 128 bit nonces

          }

          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          when(asconFastCtrl.io.in_cmdstream.fire) {
            asconInputCmdValidReg := False
          }

          dataInFifo.io.push.data.last := io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.last && io.sdramAxi.readRsp.id === axiSharedCmdReg.id && decryptVerifyCounter.willOverflowIfInc

          dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid && io.sdramAxi.readRsp.id === axiSharedCmdReg.id && (decryptVerifyCounter < 4 || decryptVerifyCounter >= 8)
          dataInFifo.io.push.data.fragment := io.sdramAxi.readRsp.data

          tagSetAsideFifo.io.push.valid := io.sdramAxi.readRsp.valid && io.sdramAxi.readRsp.id === axiSharedCmdReg.id && decryptVerifyCounter >= 4 && decryptVerifyCounter < 8
          tagSetAsideFifo.io.push.payload := io.sdramAxi.readRsp.data

          tagSetAsideFifo.io.pop.ready := asconFastCtrl.io.out_datastream.fire

          when(io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.id === axiSharedCmdReg.id) {
            when(decryptVerifyCounter.value >= 8) {
              nonceVecReg((decryptVerifyCounter.value - 8).resized) := io.sdramAxi.readRsp.data
            }

            when(io.sdramAxi.readRsp.last) {
              when(decryptVerifyCounter.willOverflowIfInc) {
                verifyTagStateReadCompleteReg := True
              } otherwise {
                sdramAxiSharedCmdValidReg := True
                decryptVerifyCounter.increment()
              }
            } otherwise {
              decryptVerifyCounter.increment()
            }
          }

          when(asconFastCtrl.io.out_datastream.fire) {
            when(asconFastCtrl.io.out_datastream.data.fragment === tagSetAsideFifo.io.pop.payload ||
                  tagSetAsideFifo.io.pop.payload === 0) {
              tagPartsVerifiedCounter.increment()
            }
          }

          when (tagPartsVerifiedCounter.willOverflowIfInc) {
            when(axiSharedCmdReg.write) {
              sdramAxiSharedCmdValidReg := True
              decryptVerifyCounter := 0

              when(!writeDataCompleteReg) {
                writeDataCompleteReg := True
                asconInputCmdValidReg := True
                goto(decryptDataState)
              } otherwise {
                sdramAxiSharedCmdValidReg := True
                sdramWrEnReg := True
                goto(writeNewTagState)
              }
            } otherwise {
              decryptVerifyCounter := 0
              updateToNextParentNodeAddrReg()
              verifyTagStateReadCompleteReg := False
              asconInputCmdValidReg := True
              sdramAxiSharedCmdValidReg := True
              when(isParentRootOfTree()) {
                layerIndexReg := numLayers - 1
                currentAddrOffsetReg := getSdramDataAddress(axiSharedCmdReg.addr)
                goto(decryptDataState)
              }
            }
          }
        }

        onExit {
          decryptVerifyCounter := 0
          tagPartsVerifiedCounter.clear()
        }
      }

      val decryptDataState: State = new State {
        onEntry {
          decryptStateReadParentNodeReg := False
          decryptStateDoneDataDecryptReg := False
        }

        whenIsActive {
          asconFastCtrl.io.in_cmdstream.mode := asconFastCtrl.DECRYPT

          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          when(!decryptStateReadParentNodeReg) {
            io.sdramAxi.sharedCmd.addr := getCurrentTagNodeAddr()
            //            io.sdramAxi.sharedCmd.len := 5 // 6 * 32 == 192 bits, which is the leaf node size
          } otherwise {
            io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
            //            io.sdramAxi.sharedCmd.len := 7 // 8 * 32 == 256 bits, AKA length of a cache line
          }
          io.sdramAxi.sharedCmd.len := 7

          io.sdramAxi.writeData.valid := False
          //          val axiSharedCmd = cloneOf(io.axi.sharedCmd)
          //
          //          axiSharedCmd.addr := io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
          //          axiSharedCmd.valid := True
          //          axiSharedCmd.write := False
          //
          //          io.sdramAxi.sharedCmd <> axiSharedCmd

          dataInFifo.io.push.payload.data.last := io.sdramAxi.readRsp.last && decryptStateReadParentNodeReg
          dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid && io.sdramAxi.readRsp.id === axiSharedCmdReg.id && ((!decryptStateReadParentNodeReg && pendingWordsCounter < 4) || decryptStateReadParentNodeReg)
          //          when(axiSharedCmdReg.write && pendingWordsCounter.value === bytePosition(axiSharedCmdReg.addr)) {
          //            dataInFifo.io.push.payload.fragment := combineNewDataWithOriginal(io.sdramAxi.readRsp.data, dataReg, strbReg)
          //          } otherwise {
          dataInFifo.io.push.payload.data.fragment := io.sdramAxi.readRsp.data
          //          }
          dataOutFifo.io.push.valid := False

          dataSetAsideFifo.io.push.fragment := asconFastCtrl.io.out_datastream.data.fragment
          dataSetAsideFifo.io.push.last := asconFastCtrl.io.out_datastream.data.last
          dataSetAsideFifo.io.push.valid := asconFastCtrl.io.out_datastream.valid && !decryptStateDoneDataDecryptReg
          asconFastCtrl.io.out_datastream.ready := dataSetAsideFifo.io.push.ready || (decryptStateDoneDataDecryptReg && tagSetAsideFifo.io.pop.valid)

          tagSetAsideFifo.io.push.valid := io.sdramAxi.readRsp.valid && io.sdramAxi.readRsp.id === axiSharedCmdReg.id && pendingWordsCounter >= 4 && !decryptStateReadParentNodeReg
          tagSetAsideFifo.io.push.payload := io.sdramAxi.readRsp.data

          tagSetAsideFifo.io.pop.ready := asconFastCtrl.io.out_datastream.fire && decryptStateDoneDataDecryptReg

          when(dataInFifo.io.push.ready && io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.id === axiSharedCmdReg.id) {

            when (pendingWordsCounter.willOverflowIfInc) {
              decryptStateReadParentNodeReg := True

              when(!decryptStateReadParentNodeReg) {
                sdramAxiSharedCmdValidReg := True
              }
            }

            pendingWordsCounter.increment()
          }

          when(asconFastCtrl.io.out_datastream.fire) {
            when(!decryptStateDoneDataDecryptReg) {
              when(asconFastCtrl.io.out_datastream.data.last) {
                decryptStateDoneDataDecryptReg := True
              }
            } otherwise {
              when(asconFastCtrl.io.out_datastream.data.fragment === tagSetAsideFifo.io.pop.payload || tagSetAsideFifo.io.pop.payload === 0) {
                tagPartsVerifiedCounter.increment()
              }
            }
          }
          when (tagPartsVerifiedCounter.willOverflowIfInc) {
            when(axiSharedCmdReg.write) {
              asconInputCmdValidReg := True
              goto(encryptDataState)
            } otherwise {
              sdramWrEnReg := False
              sdramAxiSharedCmdValidReg := False
              goto(returnDataState)
            }
          }
        }

      }

      val encryptDataState: State = new State {
        onEntry {
        }
        whenIsActive {
          asconFastCtrl.io.in_cmdstream.mode := asconFastCtrl.ENCRYPT

          dataInFifo.io.push.valid := dataSetAsideFifo.io.pop.valid
          dataSetAsideFifo.io.pop.ready := dataInFifo.io.push.ready


          // Replace byte with what we want to write
          when(pendingWordsCounter.value === bytePosition(axiSharedCmdReg.addr)) {
            dataInFifo.io.push.data.fragment := combineNewDataWithOriginal(io.sdramAxi.readRsp.data, dataReg, strbReg)
            dataInFifo.io.push.data.last := dataSetAsideFifo.io.pop.last
          } otherwise {
            dataInFifo.io.push.data := dataSetAsideFifo.io.pop.payload
          }

          when(dataInFifo.io.push.fire) {
            pendingWordsCounter.increment()

            when(pendingWordsCounter.willOverflowIfInc) {
              sdramAxiSharedCmdValidReg := True
              sdramWrEnReg := True
              goto(writeDataState)
            }
          }
        }

        onExit {
          pendingWordsCounter := 0
        }
      }

      val writeDataState: State = new State {
        onEntry {
          writeDataStateDoneStoringNonceReg := False
        }

        whenIsActive {
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
          io.sdramAxi.sharedCmd.len := 7 // 8 data bursts
          io.sdramAxi.writeData.last := pendingWordsCounter.willOverflowIfInc

          io.sdramAxi.writeData.valid := writeDataStateDoneStoringNonceReg && dataOutFifo.io.pop.valid && sdramWrEnReg
          //          otherwise {
          //            io.sdramAxi.writeData.strb := "1111"
          //          }

          nextNonceTagBlockFifo.io.push.valid := False
          dataOutFifo.io.pop.ready := io.sdramAxi.writeData.ready

//          when(io.sdramAxi.writeData.fire) {
//            pendingWordsCounter.increment()
//            when(pendingWordsCounter.willOverflowIfInc) {
//              writeDataStateDoneStoringNonceReg := True
//            }
//          }

          when(!writeDataStateDoneStoringNonceReg) {
            nextNonceTagBlockFifo.io.push.valid := dataOutFifo.io.pop.valid
            nextNonceTagBlockFifo.io.push.payload := dataOutFifo.io.pop.data
            dataOutFifo.io.pop.ready := nextNonceTagBlockFifo.io.push.ready
          }

          when(dataOutFifo.io.pop.fire && dataOutFifo.io.pop.data.last) {
            when (!writeDataStateDoneStoringNonceReg) {
              writeDataStateDoneStoringNonceReg := True
            }
          }

          when (io.sdramAxi.writeRsp.fire) {
            sdramAxiSharedCmdValidReg := True
            sdramWrEnReg := True
            goto(writeNewTagState)
          }

          onExit {
            pendingWordsCounter := 0
          }
          //          sdramWrEnReg := False
          //          sdramAxiSharedCmdValidReg := False
        }
      }


      val writeNewTagState: State = new State {
        onEntry {
        }
        whenIsActive {
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }
          io.sdramAxi.sharedCmd.addr := getCurrentTagNodeAddr()
          io.sdramAxi.sharedCmd.len := 7

          io.sdramAxi.writeData.valid := nextNonceTagBlockFifo.io.pop.valid && sdramWrEnReg
          io.sdramAxi.writeData.data := nextNonceTagBlockFifo.io.pop.payload.fragment
          io.sdramAxi.writeData.last := pendingWordsCounter.willOverflowIfInc
          io.sdramAxi.writeData.strb := "1111"
          nextNonceTagBlockFifo.io.pop.ready := io.sdramAxi.writeData.ready

          nextNonceTagBlockFifo.io.push.valid := dataOutFifo.io.pop.valid
          nextNonceTagBlockFifo.io.push.payload := dataOutFifo.io.pop.data
          dataOutFifo.io.pop.ready := nextNonceTagBlockFifo.io.push.ready

          when(io.sdramAxi.writeData.fire) {
            when(pendingWordsCounter.value < 4) {
              tempNonceReg( pendingWordsCounter.value(1 downto 0)) := nextNonceTagBlockFifo.io.pop.payload.fragment
            }
            pendingWordsCounter.increment()
          }

          when(io.sdramAxi.writeRsp.fire) {
            nonceVecReg((getSiblingIndex() << 2).resized) := tempNonceReg(0)
            nonceVecReg(((getSiblingIndex() << 2) + 1).resized) := tempNonceReg(1)
            nonceVecReg(((getSiblingIndex() << 2) + 2).resized) := tempNonceReg(2)
            nonceVecReg(((getSiblingIndex() << 2) + 3).resized) := tempNonceReg(3)

            when(isRootOfTree()) {
              busyReg := False
              sdramWrEnReg := False
              sdramAxiSharedCmdValidReg := False
              goto(idleState)
            } otherwise {
              asconInputCmdValidReg := True
              goto(calculateNewTagState)
            }
            //            pendingWordsCounter.increment()
            //            sdramAxiSharedCmdValidReg := True
            //            when(pendingWordsCounter.willOverflowIfInc) {
            //            }
          }
        }

        onExit {
          pendingWordsCounter := 0
        }
      }

      val calculateNewTagState: State = new State {
        onEntry {
          calculateNewTagStateDataInFifoValidReg := True
          calculateNewTagStateDoneReceivingNonceReg := False
        }

        whenIsActive {
          dataInFifo.io.push.data.fragment := nonceVecReg(pendingNonceCounter)
          dataInFifo.io.push.data.last := pendingNonceCounter.willOverflowIfInc
          dataInFifo.io.push.valid := calculateNewTagStateDataInFifoValidReg

          asconFastCtrl.io.in_cmdstream.mode := asconFastCtrl.MACGEN
          //          io.sdramAxi.writeData.valid := False

          when(dataInFifo.io.push.fire) {
            pendingNonceCounter.increment()

            when(pendingNonceCounter.willOverflowIfInc) {
              calculateNewTagStateDataInFifoValidReg := False
            }
          }

          dataOutFifo.io.push.valid := False

          nextNonceTagBlockFifo.io.push.valid := asconFastCtrl.io.out_datastream.valid
          nextNonceTagBlockFifo.io.push.payload := asconFastCtrl.io.out_datastream.data
          asconFastCtrl.io.out_datastream.ready := nextNonceTagBlockFifo.io.push.ready

          when(asconFastCtrl.io.out_datastream.fire && asconFastCtrl.io.out_datastream.data.last) {
            when (!calculateNewTagStateDoneReceivingNonceReg) {
              calculateNewTagStateDoneReceivingNonceReg := True
            } otherwise {
              updateToNextParentNodeAddrReg()
              when(isParentRootOfTree()) {
                sdramAxiSharedCmdValidReg := True
                sdramWrEnReg := True
                goto(writeNewTagState)
              } otherwise {
                sdramAxiSharedCmdValidReg := True
                asconInputCmdValidReg := True
                sdramWrEnReg := False
                goto(verifyTagFromSdramState)
              }
            }
          }
        }

        onExit {
          pendingNonceCounter := 0
        }
      }
      // State for returning data to dcache
      val returnDataState: State = new State {
        onEntry {
        }
        whenIsActive {
          io.axi.readRsp.valid := dataSetAsideFifo.io.pop.valid
          io.axi.readRsp.data := dataSetAsideFifo.io.pop.payload.fragment
          io.axi.readRsp.last := dataSetAsideFifo.io.pop.payload.last
          io.axi.readRsp.id := axiSharedCmdReg.id
          io.axi.readRsp.setOKAY()

          dataSetAsideFifo.io.pop.ready := io.axi.readRsp.ready

          when(io.axi.readRsp.fire  && io.axi.readRsp.last) {
            goto(idleState)
          }
        }
        onExit {
          sdramWrEnReg := False
          sdramAxiSharedCmdValidReg := False
          busyReg := False
        }
      }
    }
//  }
}