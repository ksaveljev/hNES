{-# LANGUAGE ViewPatterns #-}
module NES.Emulator (emulate
                    , loadProgram
                    ) where

import qualified Data.ByteString as B

import NES.Instruction
import NES.MonadEmulator

emulate :: MonadEmulator m => m ()
emulate = undefined

loadProgram :: MonadEmulator m => B.ByteString -> m ()
loadProgram program = undefined

execute :: MonadEmulator m => Instruction -> m ()
execute instruction@(Instruction (viewOpCode -> OpCode w8 mnemonic amode) _) = undefined
