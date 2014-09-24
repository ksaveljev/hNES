module NES.MonadEmulator ( MonadEmulator
                         ) where

import NES.CPU (Address, MemoryValue)

class (Functor m, Monad m) => MonadEmulator m where
    load8 :: Address -> m Word8
    store8 :: Address -> Word8 -> m ()
    load16 :: Address -> m Word16
    store16 :: Address -> Word16 -> m ()
    getFlag :: Flag -> m Bool
    setFlag :: Flag -> Bool -> m ()
