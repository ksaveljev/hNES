module NES.Emulator (emulate
                    , loadProgram
                    , decodeInstruction
                    , execute
                    ) where

import Data.Word (Word8, Word16)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor, testBit, complement)
import Control.Monad (unless, when, void)
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
                  let addr = x + w8
                  l <- load8 $ Ram $ fromIntegral addr
                  h <- load8 $ Ram $ fromIntegral $ addr + 1
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

samePage :: Word16 -> Word16 -> Bool
samePage a b = (a .&. 0xFF00) == (b .&. 0xFF00)

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
        let pc' = pc + fromIntegral (makeSigned v)
            pageCarry = not carry && not (samePage pc pc')
            penalty = bToW8 (not carry) + bToW8 pageCarry
        unless carry $ storePC pc'
        alterCpuCycles $ cycles + penalty
      BCS -> do
        v <- loadStorageValue8 instruction
        carry <- getCarryFlag
        pc <- loadPC
        let pc' = pc + fromIntegral (makeSigned v)
            pageCarry = carry && not (samePage pc pc')
            penalty = bToW8 carry + bToW8 pageCarry
        when carry $ storePC pc'
        alterCpuCycles $ cycles + penalty
      BEQ -> do
        v <- loadStorageValue8 instruction
        zero <- getZeroFlag
        pc <- loadPC
        let pc' = pc + fromIntegral (makeSigned v)
            pageCarry = zero && not (samePage pc pc')
            penalty = bToW8 zero + bToW8 pageCarry
        when zero $ storePC pc'
        alterCpuCycles $ cycles + penalty
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
        let pc' = pc + fromIntegral (makeSigned v)
            pageCarry = negative && not (samePage pc pc')
            penalty = bToW8 negative + bToW8 pageCarry
        when negative $ storePC pc'
        alterCpuCycles $ cycles + penalty
      BNE -> do
        v <- loadStorageValue8 instruction
        zero <- getZeroFlag
        pc <- loadPC
        let pc' = pc + fromIntegral (makeSigned v)
            pageCarry = not zero && not (samePage pc pc')
            penalty = bToW8 (not zero) + bToW8 pageCarry
        unless zero $ storePC pc'
        alterCpuCycles $ cycles + penalty
      BPL -> do
        v <- loadStorageValue8 instruction
        negative <- getNegativeFlag
        pc <- loadPC
        let pc' = pc + fromIntegral (makeSigned v)
            pageCarry = not negative && not (samePage pc pc')
            penalty = bToW8 (not negative) + bToW8 pageCarry
        unless negative $ storePC pc'
        alterCpuCycles $ cycles + penalty
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
        let pc' = pc + fromIntegral (makeSigned v)
            pageCarry = not overflow && not (samePage pc pc')
            penalty = bToW8 (not overflow) + bToW8 pageCarry
        unless overflow $ storePC pc'
        alterCpuCycles $ cycles + penalty
      BVS -> do
        v <- loadStorageValue8 instruction
        overflow <- getOverflowFlag
        pc <- loadPC
        let pc' = pc + fromIntegral (makeSigned v)
            pageCarry = overflow && not (samePage pc pc')
            penalty = bToW8 overflow + bToW8 pageCarry
        when overflow $ storePC pc'
        alterCpuCycles $ cycles + penalty
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
      ASO -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let shiftedResult = v `shiftL` 1
        storeStorageValue8 instruction shiftedResult
        let result = shiftedResult .|. a
        storeA result
        setCarryFlag $ testBit v 7
        setZNFlags result
      RLA -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        carry <- bToW8 <$> getCarryFlag
        let rotated = (v `shiftL` 1) .|. carry
        storeStorageValue8 instruction rotated
        let result = rotated .&. a
        storeA result
        setCarryFlag $ testBit v 7
        setZNFlags result
      LSE -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let shiftedResult = v `shiftR` 1
        storeStorageValue8 instruction shiftedResult
        let result = shiftedResult `xor` a
        storeA result
        setCarryFlag $ testBit v 0
        setZNFlags result
      RRA -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        carry <- getCarryFlag
        let rotated = (v `shiftR` 1) .|. if carry then 0x80 else 0
        storeStorageValue8 instruction rotated
        let result = rotated + a + bToW8 (testBit v 0)
        storeA result
        setCarryFlag $ if testBit v 0 then result <= rotated else result < rotated
        setOverflowFlag $ isOverflow a rotated result
        setZNFlags result
      AXS -> do
        _ <- loadStorageValue8 instruction
        a <- loadA
        x <- loadX
        let result = a .&. x
        storeStorageValue8 instruction result
      LAX -> do
        v <- loadStorageValue8 instruction
        storeA v
        storeX v
        setZNFlags v
      DCM -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = v - 1
        storeStorageValue8 instruction result
        setNegativeFlag $ a - result
        setZeroFlag $ a - result
        setCarryFlag $ a >= result
      INS -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        carry <- bToW8 <$> getCarryFlag
        let updated = v + 1
        storeStorageValue8 instruction updated
        let result = a - updated - (1 - carry)
        storeA result
        setCarryFlag $ if carry == 1 then result <= a else result < a
        setOverflowFlag $ isOverflow a (complement updated) result
        setZNFlags result
      ALR -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = v .&. a
            resultShifted = result `shiftR` 1
            carry = testBit result 0
        setCarryFlag carry
        setZNFlags resultShifted
      ARR -> error "opcode not implemented"
      XAA -> error "opcode not implemented"
      OAL -> error "opcode not implemented"
      SAX -> error "opcode not implemented"
      SKB -> void $ loadStorageValue8 instruction
      SKW -> void $ loadStorageValue8 instruction
      HLT -> error "opcode not implemented"
      TAS -> error "opcode not implemented"
      SAY -> error "opcode not implemented"
      XAS -> error "opcode not implemented"
      AXA -> error "opcode not implemented"
      ANC -> do
        v <- loadStorageValue8 instruction
        a <- loadA
        let result = v .&. a
        storeA result
        setCarryFlag $ testBit result 7
        setZNFlags result
      LAS -> do
        v <- loadStorageValue8 instruction
        sp <- loadSP
        let result = v .&. sp
        storeSP result
        storeA result
        storeX result
        setZNFlags result
