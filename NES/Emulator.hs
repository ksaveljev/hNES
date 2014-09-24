module NES.Emulator (emulate
                    , loadProgram
                    ) where

import Data.Word (Word8, Word16)
import Control.Applicative ((<$>))
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

decodeInstruction :: MonadEmulator m => m Instruction
decodeInstruction = do
    opCode <- loadNextWord8
    let (mn, am) = decodeOpCode opCode
    Instruction mn am <$> case operandLength am of
                            1 -> sequence [loadNextWord8] 
                            2 -> sequence [loadNextWord8, loadNextWord8]
                            _ -> return []

execute :: MonadEmulator m => Instruction -> m ()
execute (Instruction ADC am reg) = do
    a <- load8 A
    undefined
