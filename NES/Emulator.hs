module NES.Emulator (emulate
                    , loadProgram
                    ) where

import Data.Word (Word8, Word16)
import qualified Data.ByteString as B

import NES.CPU
import NES.Instruction
import NES.MonadEmulator

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

loadInstructionArgument :: MonadEmulator m => AddressingMode -> m InstructionArgument
loadInstructionArgument addressingMode =
    case addressingMode of
      Implicit -> return Empty
      Accumulator -> return Empty
      Absolute _ -> do
        w16 <- loadNextWord16
        return $ Address w16
      Indirect _ -> do
        w16 <- loadNextWord16
        return $ Address w16
      _ -> do
        w8 <- loadNextWord8
        return $ Operand w8

decodeInstruction :: MonadEmulator m => m Instruction
decodeInstruction = do
    opCode <- loadNextWord8
    let (mn, am) = decodeOpCode opCode
    arg <- loadInstructionArgument am
    return $ Instruction mn am arg

execute :: MonadEmulator m => Instruction -> m ()
execute = undefined
