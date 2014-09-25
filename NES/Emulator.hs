module NES.Emulator (emulate
                    , loadProgram
                    ) where

import Data.Word (Word8, Word16)
import Data.Bits ((.&.), shiftL, shiftR, testBit)
import Control.Monad (unless, when)
import Control.Applicative ((<$>))
import qualified Data.ByteString as B

import NES.CPU
import NES.Instruction
import NES.MonadEmulator
import NES.EmulatorHelpers
import NES.Util

emulate :: MonadEmulator m => m ()
emulate = undefined

loadProgram :: MonadEmulator m => B.ByteString -> m ()
loadProgram program = undefined

loadNextWord8 :: MonadEmulator m => m Word8
loadNextWord8 = do
    pc <- load16 Pc
    w8 <- load8 $ Ram pc
    store16 Pc (pc + 1)
    return w8

loadNextWord16 :: MonadEmulator m => m Word16
loadNextWord16 = do
    pc <- load16 Pc
    w16 <- load16 $ Ram pc
    store16 Pc (pc + 2)
    return w16

decodeInstruction :: MonadEmulator m => m Instruction
decodeInstruction = do
    opCode <- loadNextWord8
    let (mn, am) = decodeOpCode opCode
    Instruction mn am <$> case operandLength am of
                            1 -> sequence [loadNextWord8] 
                            2 -> sequence [loadNextWord8, loadNextWord8]
                            _ -> return []

getStorageAddr :: MonadEmulator m => Instruction -> m Storage
getStorageAddr instruction@(Instruction mn am arg) =
    case arg of
      [] -> case am of
              Accumulator -> return A
              _ -> oops
      [w8] -> case am of
                ZeroPage -> return $ Ram $ fromIntegral w8
                ZeroPageX -> do
                  x <- load8 X
                  return $ Ram $ fromIntegral $ w8 + x
                ZeroPageY -> do
                  y <- load8 Y
                  return $ Ram $ fromIntegral $ w8 + y
                IndexedIndirect -> do
                  x <- load8 X
                  let addr = fromIntegral $ x + w8 :: Word16
                  l <- load8 $ Ram addr
                  h <- load8 $ Ram $ addr + 1
                  return $ Ram $ makeW16 h l
                IndirectIndexed -> do
                  y <- load8 Y
                  l <- load8 $ Ram $ fromIntegral w8
                  h <- load8 $ Ram $ fromIntegral $ w8 + 1
                  return $ Ram $ makeW16 h l + fromIntegral y
                _ -> oops
      [l,h] -> case am of
                 Absolute -> return $ Ram $ makeW16 h l
                 AbsoluteX -> do
                   x <- load8 X
                   return $ Ram $ makeW16 h l + fromIntegral x
                 AbsoluteY -> do
                   y <- load8 Y
                   return $ Ram $ makeW16 h l + fromIntegral y
                 _ -> oops
      _ -> error "Incorrect Instruction argument in getStorageAddr"
      where
        oops = error "Incorrect Instruction in getStorageAddr"

loadStorageValue8 :: MonadEmulator m => Instruction -> m Word8
loadStorageValue8 instruction@(Instruction mn am arg) =
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
loadStorageValue16 instruction@(Instruction mn am arg) =
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

execute :: MonadEmulator m => Instruction -> m ()
execute instruction@(Instruction mv am arg) =
    case mv of
      ADC -> do
        v <- loadStorageValue8 instruction
        a <- load8 A
        carry <- getFlag CF
        let result = v + a + bToW8 carry
        store8 A result
        setCarryFlag $ if carry then result <= v else result < v
        setZeroFlag result
        setOverflowFlag $ isOverflow a v result
        setNegativeFlag result
      AND -> do
        v <- loadStorageValue8 instruction
        a <- load8 A
        let result = v .&. a
        store8 A result
        setZeroFlag result
        setNegativeFlag result
      ASL -> do
        v <- loadStorageValue8 instruction
        let result = v `shiftL` 1
        setCarryFlag $ testBit v 7
        setZeroFlag result
        setNegativeFlag result
        storeStorageValue8 instruction result
      BCC -> do
        v <- loadStorageValue8 instruction
        carry <- getFlag CF
        pc <- load16 Pc
        unless carry $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BCS -> do
        v <- loadStorageValue8 instruction
        carry <- getFlag CF
        pc <- load16 Pc
        when carry $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BEQ -> do
        v <- loadStorageValue8 instruction
        zero <- getFlag ZF
        pc <- load16 Pc
        when zero $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BIT -> do
        v <- loadStorageValue8 instruction
        a <- load8 A
        setZeroFlag (v .&. a)
        setOverflowFlag $ testBit v 6
        setNegativeFlag v
      BMI -> do
        v <- loadStorageValue8 instruction
        negative <- getFlag NF
        pc <- load16 Pc
        when negative $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BNE -> do
        v <- loadStorageValue8 instruction
        zero <- getFlag ZF
        pc <- load16 Pc
        unless zero $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BPL -> do
        v <- loadStorageValue8 instruction
        negative <- getFlag NF
        pc <- load16 Pc
        unless negative $ store16 Pc $ pc + fromIntegral (makeSigned v)
      BRK -> do
        pc <- loadPC
        push $ fromIntegral $ pc `shiftR` 8
        push $ fromIntegral pc
        load8 SR >>= push
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
        setZeroFlag result
        setNegativeFlag result
      CPX -> do
        v <- loadStorageValue8 instruction
        x <- loadX
        let result = x - v
        setCarryFlag $ x >= v
        setZeroFlag result
        setNegativeFlag result
      CPY -> do
        v <- loadStorageValue8 instruction
        y <- loadY
        let result = y - v
        setCarryFlag $ y >= v
        setZeroFlag result
        setNegativeFlag result
      DEC -> undefined
      DEX -> undefined
      DEY -> undefined
      EOR -> undefined
      INC -> undefined
      INX -> undefined
      INY -> undefined
      JMP -> undefined
      JSR -> undefined
      LDA -> undefined
      LDX -> undefined
      LDY -> undefined
      LSR -> undefined
      NOP -> undefined
      ORA -> undefined
      PHA -> undefined
      PHP -> undefined
      PLA -> undefined
      PLP -> undefined
      ROL -> undefined
      ROR -> undefined
      RTI -> undefined
      RTS -> undefined
      SBC -> undefined
      SEC -> undefined
      SED -> undefined
      SEI -> undefined
      STA -> undefined
      STX -> undefined
      STY -> undefined
      TAX -> undefined
      TAY -> undefined
      TSX -> undefined
      TXA -> undefined
      TXS -> undefined
      TYA -> undefined
