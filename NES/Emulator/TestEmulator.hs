{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module NES.Emulator.TestEmulator where

import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Applicative (Applicative)

import NES.VM (VM(..))
import NES.ROM (ROM)
import qualified NES.VM as VM
import qualified NES.CPU as CPU
import NES.MonadEmulator

newtype TestEmulator s a = TestEmulator (ReaderT (VM s) (ST s) a)
                           deriving (Functor, Applicative, Monad)

instance MonadEmulator (TestEmulator s) where
    load8 storage = TestEmulator $ do
      vm <- ask
      lift $ VM.load8 vm storage
    store8 storage w8 = TestEmulator $ do
      vm <- ask
      lift $ VM.store8 vm storage w8
    load16 storage = TestEmulator $ do
      vm <- ask
      lift $ VM.load16 vm storage
    store16 storage w16 = TestEmulator $ do
      vm <- ask
      lift $ VM.store16 vm storage w16
    getFlag flag = TestEmulator $ do
      vm <- ask
      lift $ CPU.getFlag (cpu vm) flag
    setFlag flag b = TestEmulator $ do
      vm <- ask
      lift $ CPU.setFlag (cpu vm) flag b
    getCpuCycles = TestEmulator $ do
      vm <- ask
      lift $ CPU.getCpuCycles (cpu vm)
    setCpuCycles w64 = TestEmulator $ do
      vm <- ask
      lift $ CPU.setCpuCycles (cpu vm) w64

runTestEmulator :: ROM -> (forall s. TestEmulator s a) -> a
runTestEmulator catridge emu = runST $ run emu
    where
      run :: TestEmulator s a -> ST s a
      run (TestEmulator reader) = do
        vm <- VM.new catridge
        runReaderT reader vm
