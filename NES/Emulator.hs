module NES.Emulator (emulate
                    , loadProgram
                    ) where

import Data.Word (Word8, Word16)
import Control.Applicative ((<$>))
import qualified Data.ByteString as B

import NES.CPU
import NES.Instruction
import NES.MonadEmulator
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

loadMemoryValue8 :: MonadEmulator m => Instruction -> m Word8
loadMemoryValue8 instruction@(Instruction mn am arg) = do
    case arg of
      [] -> case am of
              Implicit -> error "loadMemoryValue8 for Implicit addressing mode"
              Accumulator -> load8 A
              _ -> oops
      [w8] -> case am of
                Immediate -> return w8
                ZeroPage -> load8 $ Ram $ fromIntegral w8
                ZeroPageX -> do
                  x <- load8 X
                  load8 $ Ram $ fromIntegral $ w8 + x
                ZeroPageY -> do
                  y <- load8 Y
                  load8 $ Ram $ fromIntegral $ w8 + y
                Relative -> return w8
                IndexedIndirect -> do
                  x <- load8 X
                  let addr = fromIntegral $ x + w8 :: Word16
                  l <- load8 $ Ram addr
                  h <- load8 $ Ram $ addr + 1
                  load8 $ Ram $ makeW16 h l
                IndirectIndexed -> do
                  y <- load8 Y
                  l <- load8 $ Ram $ fromIntegral w8
                  h <- load8 $ Ram $ fromIntegral $ w8 + 1
                  load8 $ Ram $ makeW16 h l + fromIntegral y
                _ -> oops
      (l:h:[]) -> case am of
                    Absolute -> load8 $ Ram $ makeW16 h l
                    AbsoluteX -> do
                      x <- load8 X
                      load8 $ Ram $ makeW16 h l + fromIntegral x
                    AbsoluteY -> do
                      y <- load8 Y
                      load8 $ Ram $ makeW16 h l + fromIntegral y
                    _ -> oops
      _ -> error "Incorrect Instruction in loadMemoryValue8"
      where
        oops = error "Incorrect Instruction argument in loadMemoryValue8"

loadMemoryValue16 :: MonadEmulator m => Instruction -> m Word16
loadMemoryValue16 instruction@(Instruction mn am arg) =
    case arg of
      (l:h:[]) -> case am of
                    Absolute -> return $ makeW16 h l
                    Indirect -> do
                      low <- load8 $ Ram $ makeW16 h l
                      high <- load8 $ Ram $ makeW16 h (l + 1) -- 6502 bug
                      return $ makeW16 high low
                    _ -> error "Incorrect Instruction in loadMemoryValue16"
      _ -> error "Incorrect Instruction argument in loadMemoryValue16"

setNegativeFlag :: MonadEmulator m => Bool -> m ()
setNegativeFlag = setFlag NF

setCarryFlag :: MonadEmulator m => Bool -> m ()
setCarryFlag = setFlag CF

setOverflowFlag :: MonadEmulator m => Bool -> m ()
setOverflowFlag = setFlag OF

setZeroFlag :: MonadEmulator m => Bool -> m ()
setZeroFlag = setFlag ZF

execute :: MonadEmulator m => Instruction -> m ()
execute instruction@(Instruction mv am arg) = do
    case mv of
      ADC -> do
        v <- loadMemoryValue8 instruction
        a <- load8 A
        carry <- getFlag CF
        let result = v + a + bToW8 carry
        store8 A result
        setCarryFlag $ if carry then result <= v else result < v
        setZeroFlag $ result == 0
        setOverflowFlag $ isOverflow a v result
        setNegativeFlag $ isNegative result
