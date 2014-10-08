module NES.Emulator (emulate
                    , loadProgram
                    , decodeInstruction
                    , execute
                    ) where

import Data.Word (Word8, Word16)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor, testBit, complement)
import Control.Monad (unless, when)
import Control.Applicative ((<$>))
import qualified Data.ByteString as B

import NES.VM (Storage(..))
import NES.ROM
import NES.Instruction
import NES.MonadEmulator
import NES.EmulatorHelpers
import NES.Util

-- TODO: execute forever
emulate :: MonadEmulator m => m ()
emulate = do
    instruction <- decodeInstruction
    execute instruction

-- TODO: this is no longer valid
loadProgram :: MonadEmulator m => B.ByteString -> m ()
loadProgram program = do
    -- TODO: Currently support PRG-ROM with only 1 ROM page
    when (prgROMsize (header rom) > 1) $ error "PRG-ROM has more than 1 ROM page!"
    mapM_ (loadPrgROM 0x8000) [0..B.length prg - 1]
    mapM_ (loadPrgROM 0xC000) [0..B.length prg - 1]
    where
      rom = loadROM program
      prg = prgROM rom
      loadPrgROM offset i =
        let byte = B.index prg i
            address = offset + fromIntegral i
        in store8 (Ram address) byte

loadNextWord8 :: MonadEmulator m => m Word8
loadNextWord8 = do
    pc <- loadPC
    w8 <- load8 $ Ram pc
    storePC (pc + 1)
    return w8

decodeInstruction :: MonadEmulator m => m Instruction
decodeInstruction = do
    opCode <- loadNextWord8
    let (mn, am, cycles) = decodeOpCode opCode
    Instruction opCode cycles mn am <$> case operandLength am of
                                          1 -> sequence [loadNextWord8] 
                                          2 -> sequence [loadNextWord8, loadNextWord8]
                                          _ -> return []

getStorageAddr :: MonadEmulator m => Instruction -> m Storage
getStorageAddr (Instruction _ _ _ am arg) =
    case arg of
      [] -> case am of
              Accumulator -> return A
              _ -> oops
      [w8] -> case am of
                ZeroPage -> return $ Ram $ fromIntegral w8
                ZeroPageX -> do
                  x <- loadX
                  return $ Ram $ fromIntegral $ w8 + x
                ZeroPageY -> do
                  y <- loadY
                  return $ Ram $ fromIntegral $ w8 + y
                IndexedIndirect -> do
                  x <- loadX
                  let addr = fromIntegral $ x + w8 :: Word16
                  l <- load8 $ Ram addr
                  h <- load8 $ Ram $ addr + 1
                  return $ Ram $ makeW16 h l
                IndirectIndexed -> do
                  y <- loadY
                  l <- load8 $ Ram $ fromIntegral w8
                  h <- load8 $ Ram $ fromIntegral $ w8 + 1
                  return $ Ram $ makeW16 h l + fromIntegral y
                _ -> oops
      [l,h] -> case am of
                 Absolute -> return $ Ram $ makeW16 h l
                 AbsoluteX -> do
                   x <- loadX
                   return $ Ram $ makeW16 h l + fromIntegral x
                 AbsoluteY -> do
                   y <- loadY
                   return $ Ram $ makeW16 h l + fromIntegral y
                 _ -> oops
      _ -> error "Incorrect Instruction argument in getStorageAddr"
      where
        oops = error "Incorrect Instruction in getStorageAddr"

loadStorageValue8 :: MonadEmulator m => Instruction -> m Word8
loadStorageValue8 instruction@(Instruction _ _ _ am arg) =
    case arg of
      [w8] -> case am of
                Immediate -> return w8
                Relative -> return w8
                _ -> load
      _ -> load
  where
    load = do
      addr <- getStorageAddr instruction
      load8 addr

loadStorageValue16 :: MonadEmulator m => Instruction -> m Word16
loadStorageValue16 (Instruction _ _ _ am arg) =
    case arg of
      [l,h] -> case am of
                 Absolute -> return $ makeW16 h l
                 Indirect -> do
                   low <- load8 $ Ram $ makeW16 h l
                   high <- load8 $ Ram $ makeW16 h (l + 1) -- 6502 bug
                   return $ makeW16 high low
                 _ -> error "Incorrect Instruction in loadStorageValue16"
      _ -> error "Incorrect Instruction argument in loadStorageValue16"

storeStorageValue8 :: MonadEmulator m => Instruction -> Word8 -> m ()
storeStorageValue8 instruction w8 = getStorageAddr instruction >>= (`store8` w8)

alterStorageValue8 :: MonadEmulator m => Instruction -> (Word8 -> Word8) -> m Word8
alterStorageValue8 instruction f = do
    v <- loadStorageValue8 instruction
    let w8 = f v
    storeStorageValue8 instruction w8
    return w8

pageCrossPenalty :: MonadEmulator m => Instruction -> m Cycles
pageCrossPenalty (Instruction _ _ _ am arg) =
    case arg of
      [w8] -> case am of
                IndirectIndexed -> do
                  v <- load8 . Ram . fromIntegral $ w8
                  y <- loadY
                  return $ if v + y < v then 1 else 0
                _ -> return 0
      [l,_] -> case am of
                 AbsoluteX -> do
                   x <- loadX
                   return $ if l + x < l then 1 else 0
                 AbsoluteY -> do
                   y <- loadY
                   return $ if l + y < l then 1 else 0
                 _ -> return 0
      _ -> return 0

execute :: MonadEmulator m => Instruction -> m ()
execute instruction@(Instruction _ cycles mv _ _) =
    case mv of
      ADC -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        carry <- getCarryFlag
        let result = v + a + bToW8 carry
        storeA result
        setCarryFlag $ if carry then result <= v else result < v
        setOverflowFlag $ isOverflow a v result
        setZNFlags result
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      AND -> do
        loadStorageValue8 instruction >>= alterA . (.&.) >>= setZNFlags
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      ASL -> do
        v <- loadStorageValue8 instruction
        let result = v `shiftL` 1
        storeStorageValue8 instruction result
        setCarryFlag $ testBit v 7
        setZNFlags result
      BCC -> do
        v <- loadStorageValue8 instruction
        carry <- getCarryFlag
        pc <- loadPC
        unless carry $ storePC $ pc + fromIntegral (makeSigned v)
      BCS -> do
        v <- loadStorageValue8 instruction
        carry <- getCarryFlag
        pc <- loadPC
        when carry $ storePC $ pc + fromIntegral (makeSigned v)
      BEQ -> do
        v <- loadStorageValue8 instruction
        zero <- getZeroFlag
        pc <- loadPC
        when zero $ storePC $ pc + fromIntegral (makeSigned v)
      BIT -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        setZeroFlag (v .&. a)
        setOverflowFlag $ testBit v 6
        setNegativeFlag v
      BMI -> do
        v <- loadStorageValue8 instruction
        negative <- getNegativeFlag
        pc <- loadPC
        when negative $ storePC $ pc + fromIntegral (makeSigned v)
      BNE -> do
        v <- loadStorageValue8 instruction
        zero <- getZeroFlag
        pc <- loadPC
        unless zero $ storePC $ pc + fromIntegral (makeSigned v)
      BPL -> do
        v <- loadStorageValue8 instruction
        negative <- getNegativeFlag
        pc <- loadPC
        unless negative $ storePC $ pc + fromIntegral (makeSigned v)
      BRK -> do
        pc <- loadPC
        push $ fromIntegral $ pc `shiftR` 8
        push $ fromIntegral pc
        setBFlag True
        loadSR >>= push
        setBreakCommandFlag True
        low <- loadRAM 0xFFFE
        high <- loadRAM 0xFFFF
        storePC $ makeW16 high low
      BVC -> do
        v <- loadStorageValue8 instruction
        overflow <- getOverflowFlag
        pc <- loadPC
        unless overflow $ storePC $ pc + fromIntegral (makeSigned v)
      BVS -> do
        v <- loadStorageValue8 instruction
        overflow <- getOverflowFlag
        pc <- loadPC
        when overflow $ storePC $ pc + fromIntegral (makeSigned v)
      CLC -> setCarryFlag False
      CLD -> setDecimalModeFlag False
      CLI -> setInterruptDisableFlag False
      CLV -> setOverflowFlag False
      CMP -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = a - v
        setCarryFlag $ a >= v
        setZNFlags result
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      CPX -> do
        v <- loadStorageValue8 instruction
        x <- loadX
        let result = x - v
        setCarryFlag $ x >= v
        setZNFlags result
      CPY -> do
        v <- loadStorageValue8 instruction
        y <- loadY
        let result = y - v
        setCarryFlag $ y >= v
        setZNFlags result
      DEC -> alterStorageValue8 instruction (subtract 1) >>= setZNFlags
      DEX -> alterX (subtract 1) >>= setZNFlags
      DEY -> alterY (subtract 1) >>= setZNFlags
      EOR -> do
        loadStorageValue8 instruction >>= alterA . xor >>= setZNFlags
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      INC -> alterStorageValue8 instruction (+1) >>= setZNFlags
      INX -> alterX (+1) >>= setZNFlags
      INY -> alterY (+1) >>= setZNFlags
      JMP -> loadStorageValue16 instruction >>= storePC
      JSR -> do
        addr <- loadStorageValue16 instruction
        pc <- loadPC
        let pc' = pc - 1
        push $ fromIntegral $ pc' `shiftR` 8
        push $ fromIntegral pc' 
        storePC addr
      LDA -> do
        loadStorageValue8 instruction >>= alterA . const >>= setZNFlags
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      LDX -> do
        loadStorageValue8 instruction >>= alterX . const >>= setZNFlags
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      LDY -> do
        loadStorageValue8 instruction >>= alterY . const >>= setZNFlags
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      LSR -> do
        v <- loadStorageValue8 instruction
        let result = v `shiftR` 1
        storeStorageValue8 instruction result
        setCarryFlag $ testBit v 0
        setZNFlags result
      NOP -> return ()
      ORA -> do
        loadStorageValue8 instruction >>= alterA . (.|.) >>= setZNFlags
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      PHA -> loadA >>= push
      PHP -> setBFlag True >> loadSR >>= push
      PLA -> pop >>= alterA . const >>= setZNFlags
      PLP -> pop >>= storeSR
      ROL -> do
        v <- loadStorageValue8 instruction
        carry <- bToW8 <$> getCarryFlag
        let result = (v `shiftL` 1) .|. carry
        storeStorageValue8 instruction result
        setCarryFlag $ testBit v 7
        setZNFlags result
      ROR -> do
        v <- loadStorageValue8 instruction
        carry <- getCarryFlag
        let result = (v `shiftR` 1) .|. if carry then 0x80 else 0
        storeStorageValue8 instruction result
        setCarryFlag $ testBit v 0
        setZNFlags result
      RTI -> do
        pop >>= storeSR
        pop16 >>= storePC
      RTS -> fmap (+1) pop16 >>= storePC
      SBC -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        carry <- bToW8 <$> getCarryFlag
        let result = a - v - (1 - carry)
        storeA result
        setCarryFlag $ if carry == 1 then result <= a else result < a
        setOverflowFlag $ isOverflow a (complement v) result
        setZNFlags result
        pageCrossPenalty instruction >>= alterCpuCycles . (cycles +)
      SEC -> setCarryFlag True
      SED -> setDecimalModeFlag True
      SEI -> setInterruptDisableFlag True
      STA -> loadA >>= storeStorageValue8 instruction
      STX -> loadX >>= storeStorageValue8 instruction
      STY -> loadY >>= storeStorageValue8 instruction
      TAX -> loadA >>= alterX . const >>= setZNFlags
      TAY -> loadA >>= alterY . const >>= setZNFlags
      TSX -> loadSP >>= alterX . const >>= setZNFlags
      TXA -> loadX >>= alterA . const >>= setZNFlags
      TXS -> loadX >>= storeSP
      TYA -> loadY >>= alterA . const >>= setZNFlags
      -- unofficial
      AAC -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = v .&. a
        setCarryFlag $ testBit result 7
        setZNFlags result
      AAX -> do
        _ <- loadStorageValue8 instruction
        x <- loadX
        a <- loadA
        let result = x .&. a
        storeStorageValue8 instruction result
        setZNFlags result
      ARR -> undefined
      ASR -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = v .&. a
        let shiftedResult = result `shiftR` 1
        setCarryFlag $ testBit result 0
        setZNFlags shiftedResult
      ATX -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = v .&. a
        storeX a
        setZNFlags result
      AXA -> undefined
      AXS -> undefined
      DCP -> undefined
      DOP -> do
        _ <- loadStorageValue8 instruction
        return ()
      ISC -> undefined
      KIL -> undefined
      LAR -> undefined
      LAX -> undefined
      RLA -> undefined
      RRA -> undefined
      SLO -> undefined
      SRE -> undefined
      SXA -> undefined
      SYA -> undefined
      TOP -> do
        _ <- loadStorageValue8 instruction
        return ()
      XAA -> undefined
      XAS -> undefined
