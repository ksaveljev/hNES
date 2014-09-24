module NES.MonadEmulator where

import NES.CPU (Address, MemoryValue)

class (Functor m, Monad m) => MonadEmulator m where
    load :: Address -> m MemoryValue
    store :: Address -> MemoryValue -> m ()
