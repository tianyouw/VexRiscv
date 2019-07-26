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

  // -------------Tree calculation stuff start
  final val treeAry = 4
  final val treeStart = 0x2000000 // TODO: andrew, replace this with whatever you want
  final val memorySizeBytes = 32 * 1024 * 1024 // TODO: andrew, get this from somewhere else?
  final val blockSizeBytes = 24

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

  val numLayers = getNumLayers(memorySizeBytes)
  // -------------Tree calculation stuff end
  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
    val sdramAxi = master(Axi4Shared(axiConfig))
  }

  val layerAddressVec = Vec(UInt(axiConfig.addressWidth bits), numLayers)

  var blockNum = 0
  for (layer <- 0 until numLayers) {
    layerAddressVec(layer) := treeStart +  blockNum * 24
    blockNum = getChild(blockNum, 0)
  }

  val debugFsmState = RegInit(U(0, 3 bits))
  val layerIndexReg = RegInit(U(numLayers))
  // Data = 8 bytes, tag = 4 bytes, nonce = 2 bytes
  val dataInFifo = StreamFifo(dataType = CAESARCtrlInData(axiConfig), depth = 14)
  val dataOutFifo = StreamFifo(dataType = CAESARCtrlOutData(axiConfig), depth = 14)

  val dataSetAsideFifo = StreamFifo(dataType = Fragment(Bits(axiDataWidth bits)), depth = 8)
  val nextNonceTagBlockFifo = StreamFifo(dataType = Fragment(Bits(axiConfig.dataWidth bits)), depth = 6)

  // Tree address registers
//  val currentNodeFirstSiblingStartAddrOffsetReg = Reg(UInt(axiConfig.addressWidth bits))
  val currentAddrOffsetReg = Reg(UInt(axiConfig.addressWidth bits))


  val currentLevelStartAddr = UInt(axiConfig.addressWidth bits)
  currentLevelStartAddr := layerAddressVec(layerIndexReg)

  val nonceVecReg = Vec(Reg(Bits(axiConfig.dataWidth bits)), 8)

  val decryptVerifyCounter = Counter(0 until 14)
  val pendingWordsCounter = Counter(0 until 8)

  val caesarCtrl = DummyCAESARCtrl(axiConfig)

//  val sharedCmdReg = RegNextWhen(io.axi.sharedCmd, io.axi.sharedCmd.fire)

  val sdramWrEnReg = RegInit(False)
  val writeRspValidReg = RegInit(False)
  val sdramAxiSharedCmdValidReg = RegInit(False)


  val axiSharedCmdReg = Reg(Stream(Axi4Arw(axiConfig)))
  val dataReg = Reg(Bits(axiDataWidth bits))
  val strbReg = Reg(Bits(axiConfig.bytePerWord bits))
  val busyReg = RegInit(False)
  val caesarInputCmdValidReg = RegInit(False)




  def bytePosition(addr: UInt) : UInt = addr(4 downto 2)

  def combineNewDataWithOriginal(originalData: Bits, newData: Bits, strbMask: Bits) : Bits = {
    assert(originalData.getWidth == newData.getWidth)
    assert(newData.getWidth == 32)
    val outData = newData

    when (strbMask(3)) {
      outData(31 downto 24) := newData(31 downto 24)
    }

    when (strbMask(2)) {
      outData(23 downto 16) := newData(23 downto 16)
    }

    when (strbMask(1)) {
      outData(15 downto 8) := newData(15 downto 8)
    }

    when (strbMask(0)) {
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
  def getCurrentTagNodeFirstSiblingAddr() : UInt = {
    (currentLevelStartAddr + getFirstSiblingAddrOffset(currentAddrOffsetReg)).resize(axiConfig.addressWidth)
  }

  def multiply_683(n: UInt): UInt = ((n + (n << 1)) + ((n << 3) + (n << 5))) + ((n << 7) + (n << 9))
  def multiply_96(n: UInt): UInt = (n << 5) + (n << 6)
  def divide_96(n: UInt): UInt = {
    val q = (((n(26 downto 7) + n(26 downto 9)) + (n(26 downto 11) + n(26 downto 13))) + ((n(26 downto 15) + n(26 downto 17)) + (n(26 downto 19) + n(26 downto 21)))) + (n(26 downto 23) + n(26 downto 25))
    val r = n - multiply_96(q)
    q + (multiply_683(r)(31 downto 16))
  }

  def multiply_2731(n: UInt): UInt = ((n + (n << 1)) + ((n << 3) + (n << 5))) + (((n << 7) + (n << 9)) + (n << 11))
  def multiply_24(n: UInt): UInt = (n << 3) + (n << 4)
  def divide_24(n: UInt): UInt = {
    val q = (((n(26 downto 5) + n(26 downto 7)) + (n(26 downto 9) + n(26 downto 11))) + ((n(26 downto 13) + n(26 downto 15)) + (n(26 downto 17) + n(26 downto 19)))) + ((n(26 downto 21) + n(26 downto 23)) + n(26 downto 25))
    val r = n - multiply_24(q)
    q + (multiply_2731(r)(31 downto 16))
  }
//
  def getFirstSiblingAddrOffset(addr: UInt): UInt = multiply_96(divide_96(addr))
//
//
  def getParentNodeAddrOffset() : UInt = multiply_24((divide_24(currentAddrOffsetReg) - 1) >> 2).resize(axiConfig.addressWidth)
//
  def getParentNodeAddr(): UInt = (layerAddressVec(layerIndexReg - 1) + getParentNodeAddrOffset()).resize(axiConfig.addressWidth)
//
  def isRootOfTree(): Bool = layerIndexReg === 0

  def isParentRootOfTree(): Bool = layerIndexReg === 1

  def getSiblingIndex(): UInt = {
    val blockNum = divide_24(currentAddrOffsetReg)
    val antimask = U(0x03, 22 bits)
    (blockNum - ((blockNum - 1) & ~antimask) - 1).resize(2 bits)
  }

  dataInFifo.io.push.valid := False
  dataInFifo.io.push.payload.assignDontCare()

  caesarCtrl.io.out_datastream <> dataOutFifo.io.push
  caesarCtrl.io.in_datastream <> dataInFifo.io.pop
  caesarCtrl.io.in_cmdstream.payload.assignDontCare()
//  caesarCtrl.io.in_cmdstream.valid := False

//  dataOutFifo.io.pop.ready := False

  dataSetAsideFifo.io.push.valid := False
  dataSetAsideFifo.io.push.payload.assignDontCare()
  dataSetAsideFifo.io.pop.ready := False

  nextNonceTagBlockFifo.io.push.valid := False
  nextNonceTagBlockFifo.io.push.payload.assignDontCare()
  nextNonceTagBlockFifo.io.pop.ready := False

  val writeDataCompleteReg = RegInit(False)

  val decryptStateReadDataReg = RegInit(False)

  val writeDataStateDoneWritingReg = RegInit(False)

  val calculateNewTagDataInFifoValidReg = RegInit(True)

  val verifyTagStateReadCompleteReg = RegInit(False)

  val tempNonceReg = Vec(Reg(Bits(axiConfig.dataWidth bits)), 2)

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

    caesarCtrl.io.in_cmdstream.valid := caesarInputCmdValidReg

    when(caesarCtrl.io.in_cmdstream.fire) {
      caesarInputCmdValidReg := False
    }

    val fsm = new StateMachine {
      val idleState: State = new State with EntryPoint {
        onEntry {
          pendingWordsCounter := 0
          sdramWrEnReg := False
          debugFsmState := IDLE_STATE
        }

        whenIsActive {
          when (busyReg || (io.axi.sharedCmd.valid && io.axi.sharedCmd.addr < treeStart)) {
            when(io.axi.sharedCmd.fire) {
              busyReg := True
              axiSharedCmdReg := io.axi.sharedCmd
              writeDataCompleteReg := False
              decryptStateReadDataReg := False
              writeDataStateDoneWritingReg := False
              calculateNewTagDataInFifoValidReg := True
              verifyTagStateReadCompleteReg := False

              layerIndexReg := numLayers - 1
              //            currentLevelStartAddrReg := 0x4000000
              currentAddrOffsetReg := (io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) * 24)
              //            currentNodeFirstSiblingStartAddrOffsetReg := getFirstSiblingAddrOffset(io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) * 0xC).resize(axiConfig.addressWidth)

              // For the read case, go directly to read state
              when(!io.axi.sharedCmd.write) {
                caesarInputCmdValidReg := True
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
              caesarInputCmdValidReg := True
              goto(verifyTagFromSdramState)
            }
          } otherwise {
            io.axi <> io.sdramAxi
          }
        }
      }

      val verifyTagFromSdramState: State = new State {
        onEntry {
          verifyTagStateReadCompleteReg := False
          debugFsmState := VERIFY_TAG_FROM_SDRAM_STATE
        }
        whenIsActive {
          when(isRootOfTree()) {
            when (axiSharedCmdReg.write) {
              sdramAxiSharedCmdValidReg := True
              sdramWrEnReg := True
              goto(writeNewTagState)
            } otherwise {
              goto(decryptDataState)
            }
          }

          io.sdramAxi.writeData.valid := False
          dataOutFifo.io.push.valid := False
          caesarCtrl.io.in_cmdstream.mode := caesarCtrl.MACVERIFY
          caesarCtrl.io.out_datastream.ready := verifyTagStateReadCompleteReg

          when(decryptVerifyCounter.value < 8) {
            io.sdramAxi.sharedCmd.addr := getCurrentTagNodeFirstSiblingAddr() + (decryptVerifyCounter << 2)
            io.sdramAxi.sharedCmd.len := 1 // Burst of 2, since 64 bit nonces
          } otherwise {
            io.sdramAxi.sharedCmd.addr := getParentNodeAddr()
            io.sdramAxi.sharedCmd.len := 5 // Burst of 6, since 128 bit tag + 64 bit nonce
          }

          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          when(caesarCtrl.io.in_cmdstream.fire) {
            caesarInputCmdValidReg := False
          }

          dataInFifo.io.push.payload.data.last := io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.last && decryptVerifyCounter.willOverflowIfInc

          dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid
          dataInFifo.io.push.payload.data.fragment := io.sdramAxi.readRsp.data

          when(io.sdramAxi.readRsp.fire) {
            when(decryptVerifyCounter.value < 8) {
              nonceVecReg(decryptVerifyCounter.value(2 downto 0)) := io.sdramAxi.readRsp.data
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

          when(caesarCtrl.io.out_datastream.fire && !caesarCtrl.io.out_datastream.error) {
            when(axiSharedCmdReg.write) {
              sdramAxiSharedCmdValidReg := True
              decryptVerifyCounter := 0

              when(!writeDataCompleteReg) {
                writeDataCompleteReg := True
                caesarInputCmdValidReg := True
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
              caesarInputCmdValidReg := True
              sdramAxiSharedCmdValidReg := True
              when (isParentRootOfTree()) {
                layerIndexReg := numLayers - 1
                currentAddrOffsetReg := (io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) * 24)
                goto(decryptDataState)
              }
            }
          }
        }

        onExit {
          decryptVerifyCounter := 0
        }
      }

      val decryptDataState: State = new State {
        onEntry {
          decryptStateReadDataReg := False
          debugFsmState := DECRYPT_DATA_STATE
        }

        whenIsActive {
          caesarCtrl.io.in_cmdstream.mode := caesarCtrl.DECRYPT

          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          when(!decryptStateReadDataReg) {
            io.sdramAxi.sharedCmd.addr := getCurrentTagNodeAddr()
            io.sdramAxi.sharedCmd.len := 5 // 6 * 32 == 192 bits, which is the leaf node size
          } otherwise {
            io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
            io.sdramAxi.sharedCmd.len := 7 // 8 * 32 == 256 bits, AKA length of a cache line
          }

          io.sdramAxi.writeData.valid := False
          //          val axiSharedCmd = cloneOf(io.axi.sharedCmd)
          //
          //          axiSharedCmd.addr := io.axi.sharedCmd.addr(axiConfig.addressWidth - 1 downto 5) @@ U"00000" + (pendingWordsCounter.value << 2)
          //          axiSharedCmd.valid := True
          //          axiSharedCmd.write := False
          //
          //          io.sdramAxi.sharedCmd <> axiSharedCmd

          dataInFifo.io.push.payload.data.last := io.sdramAxi.readRsp.last && decryptStateReadDataReg
          dataInFifo.io.push.valid := io.sdramAxi.readRsp.valid
          //          when(axiSharedCmdReg.write && pendingWordsCounter.value === bytePosition(axiSharedCmdReg.addr)) {
          //            dataInFifo.io.push.payload.fragment := combineNewDataWithOriginal(io.sdramAxi.readRsp.data, dataReg, strbReg)
          //          } otherwise {
          dataInFifo.io.push.payload.data.fragment := io.sdramAxi.readRsp.data
          //          }

          dataSetAsideFifo.io.push.fragment := dataOutFifo.io.pop.data.fragment
          dataSetAsideFifo.io.push.last := dataOutFifo.io.pop.data.last
          dataSetAsideFifo.io.push.valid := dataOutFifo.io.pop.valid
          dataOutFifo.io.pop.ready := dataSetAsideFifo.io.push.ready

          when(dataInFifo.io.push.ready && io.sdramAxi.readRsp.fire && io.sdramAxi.readRsp.last) {
            decryptStateReadDataReg := True

            when (!decryptStateReadDataReg) {
              sdramAxiSharedCmdValidReg := True
            }
          }

          when(decryptStateReadDataReg && dataSetAsideFifo.io.push.fire && dataSetAsideFifo.io.push.last) {
            when(axiSharedCmdReg.write) {
              caesarInputCmdValidReg := True
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
          debugFsmState := ENCRYPT_DATA_STATE
        }
        whenIsActive {
          caesarCtrl.io.in_cmdstream.mode := caesarCtrl.ENCRYPT

          dataInFifo.io.push.valid := dataSetAsideFifo.io.pop.valid
          dataSetAsideFifo.io.pop.ready := dataInFifo.io.push.ready


          // Replace byte with what we want to write
          when (pendingWordsCounter.value === bytePosition(axiSharedCmdReg.addr)) {
            dataInFifo.io.push.data.fragment := combineNewDataWithOriginal(io.sdramAxi.readRsp.data, dataReg, strbReg)
            dataInFifo.io.push.data.last := dataSetAsideFifo.io.pop.last
          } otherwise {
            dataInFifo.io.push.data := dataSetAsideFifo.io.pop.payload
          }

          when (dataInFifo.io.push.fire) {
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
          writeDataStateDoneWritingReg := False
          debugFsmState := WRITE_DATA_STATE
        }

        whenIsActive {
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }

          io.sdramAxi.sharedCmd.addr := getSdramDataAddress(axiSharedCmdReg.addr)
          io.sdramAxi.sharedCmd.len := 7 // 8 data bursts
          io.sdramAxi.writeData.last := pendingWordsCounter.willOverflowIfInc

          io.sdramAxi.writeData.valid := !writeDataStateDoneWritingReg && dataOutFifo.io.pop.valid && sdramWrEnReg
          //          otherwise {
          //            io.sdramAxi.writeData.strb := "1111"
          //          }

          when (io.sdramAxi.writeData.fire) {
            pendingWordsCounter.increment()
            when (pendingWordsCounter.willOverflowIfInc) {
              writeDataStateDoneWritingReg := True
            }
          }

          when (writeDataStateDoneWritingReg) {
            nextNonceTagBlockFifo.io.push.valid := dataOutFifo.io.pop.valid
            nextNonceTagBlockFifo.io.push.payload := dataOutFifo.io.pop.data
            dataOutFifo.io.pop.ready := nextNonceTagBlockFifo.io.push.ready

            when (dataOutFifo.io.pop.fire && dataOutFifo.io.pop.data.last) {
              sdramAxiSharedCmdValidReg := True
              sdramWrEnReg := True
              goto(writeNewTagState)
            }
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
          debugFsmState := WRITE_NEW_TAG_STATE
        }
        whenIsActive {
          when(io.sdramAxi.sharedCmd.fire) {
            sdramAxiSharedCmdValidReg := False
          }
          io.sdramAxi.sharedCmd.addr := getCurrentTagNodeAddr()
          io.sdramAxi.sharedCmd.len := 5 // 6 * 32 == 192 bits; 6 - 1 = 5

          io.sdramAxi.writeData.valid := nextNonceTagBlockFifo.io.pop.valid && sdramWrEnReg
          io.sdramAxi.writeData.data := nextNonceTagBlockFifo.io.pop.payload.fragment
          io.sdramAxi.writeData.last := nextNonceTagBlockFifo.io.pop.payload.last
          io.sdramAxi.writeData.strb := "1111"
          nextNonceTagBlockFifo.io.pop.ready := io.sdramAxi.writeData.ready

          when(io.sdramAxi.writeData.fire) {
            when (pendingWordsCounter.value === 0 || pendingWordsCounter.value === 1) {
              tempNonceReg(pendingWordsCounter.value(0 downto 0)) := nextNonceTagBlockFifo.io.pop.payload.fragment
            }
            pendingWordsCounter.increment()
          }

          when(io.sdramAxi.writeRsp.fire) {
            val index = getSiblingIndex()
            nonceVecReg((index << 1)(2 downto 0)) := tempNonceReg(0)
            nonceVecReg(((index << 1) + 1)(2 downto 0)) := tempNonceReg(1)

            when (isRootOfTree()) {
              busyReg := False
              sdramWrEnReg := False
              sdramAxiSharedCmdValidReg := False
              goto(idleState)
            } otherwise {
              caesarInputCmdValidReg := True
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
          calculateNewTagDataInFifoValidReg := True
          debugFsmState := CALCULATE_NEW_TAG_STATE
        }

        whenIsActive {
          dataInFifo.io.push.data.fragment := nonceVecReg(pendingWordsCounter)
          dataInFifo.io.push.data.last := pendingWordsCounter.willOverflowIfInc
          dataInFifo.io.push.valid := calculateNewTagDataInFifoValidReg

          caesarCtrl.io.in_cmdstream.mode := caesarCtrl.MACGEN
//          io.sdramAxi.writeData.valid := False

          when (dataInFifo.io.push.fire) {
            pendingWordsCounter.increment()

            when (pendingWordsCounter.willOverflowIfInc) {
              calculateNewTagDataInFifoValidReg := False
            }
          }

          dataOutFifo.io.push.valid := False

          nextNonceTagBlockFifo.io.push.valid := caesarCtrl.io.out_datastream.valid
          nextNonceTagBlockFifo.io.push.payload := caesarCtrl.io.out_datastream.data
          caesarCtrl.io.out_datastream.ready := nextNonceTagBlockFifo.io.push.ready

          when (caesarCtrl.io.out_datastream.fire && caesarCtrl.io.out_datastream.data.last && !caesarCtrl.io.out_datastream.error) {
            updateToNextParentNodeAddrReg()
            when (isParentRootOfTree()) {
              sdramAxiSharedCmdValidReg := True
              sdramWrEnReg := True
              goto(writeNewTagState)
            } otherwise {
              sdramAxiSharedCmdValidReg := True
              caesarInputCmdValidReg := True
              sdramWrEnReg := False
              goto(verifyTagFromSdramState)
            }
          }
        }


      }
      // State for returning data to dcache
      val returnDataState: State = new State {
        onEntry {
          debugFsmState := RETURN_DATA_STATE
        }
        whenIsActive {
          io.axi.readRsp.valid := dataSetAsideFifo.io.pop.valid
          io.axi.readRsp.data := dataSetAsideFifo.io.pop.payload.fragment
          io.axi.readRsp.last := dataSetAsideFifo.io.pop.payload.last
          io.axi.readRsp.id := axiSharedCmdReg.id
          io.axi.readRsp.setOKAY()

          dataSetAsideFifo.io.pop.ready := io.axi.readRsp.ready

          when(io.axi.readRsp.fire && io.axi.readRsp.last) {
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
