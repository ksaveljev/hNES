{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NES.Emulator.TestEmulator where

import Data.STRef (readSTRef)
import Control.Monad.ST (ST)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (lift)
import Control.Applicative (Applicative)

import NES.VM
import NES.CPU
import NES.MonadEmulator

newtype TestEmulator s a = TestEmulator (ReaderT (VM s) (ST s) a)
                         deriving (Functor, Applicative, Monad)

instance MonadEmulator (TestEmulator s) where
    load8 storage = TestEmulator $ do
      vm <- ask
      let cpu = getCPU vm
      case storage of
        Pc -> lift $ readSTRef $ programCounter cpu
        Sp -> lift $ readSTRef $ stackPointer cpu
        A -> lift $ readSTRef $ registerA cpu
        X -> lift $ readSTRef $ registerX cpu
        Y -> lift $ readSTRef $ registerY cpu
        SR -> lift $ readSTRef $ cpuFlags cpu
        Ram addr -> undefined
    store8 = undefined
    load16 = undefined
    store16 = undefined
    getFlag = undefined
    setFlag = undefined
