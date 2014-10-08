module NES.MonadEmulator ( MonadEmulator(..)
                         ) where

import Data.Word (Word8, Word16, Word64)

import NES.VM (Storage)
import NES.CPU (Flag)

class (Functor m, Monad m) => MonadEmulator m where
    load8 :: Storage -> m Word8
    store8 :: Storage -> Word8 -> m ()
    load16 :: Storage -> m Word16
    store16 :: Storage -> Word16 -> m ()
    getFlag :: Flag -> m Bool
    setFlag :: Flag -> Bool -> m ()
    getCpuCycles :: m Word64
    setCpuCycles :: Word64 -> m ()
