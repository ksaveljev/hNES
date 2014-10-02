{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module NES.Emulator.TestEmulator where

import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Applicative (Applicative)

import NES.VM (VM(..))
import qualified NES.VM as VM
import qualified NES.CPU as CPU
import NES.MonadEmulator

newtype TestEmulator s a = TestEmulator (ReaderT (VM s) (ST s) a)
                           deriving (Functor, Applicative, Monad)

instance MonadEmulator (TestEmulator s) where
    load8 storage = TestEmulator $ do
      vm <- ask
      lift $ CPU.load8 (getCPU vm) storage
    store8 storage w8 = TestEmulator $ do
      vm <- ask
      lift $ CPU.store8 (getCPU vm) storage w8
    load16 storage = TestEmulator $ do
      vm <- ask
      lift $ CPU.load16 (getCPU vm) storage
    store16 storage w16 = TestEmulator $ do
      vm <- ask
      lift $ CPU.store16 (getCPU vm) storage w16
    getFlag flag = TestEmulator $ do
      vm <- ask
      lift $ CPU.getFlag (getCPU vm) flag
    setFlag flag b = TestEmulator $ do
      vm <- ask
      lift $ CPU.setFlag (getCPU vm) flag b

runTestEmulator :: (forall s. TestEmulator s a) -> a
runTestEmulator emu = runST $ run emu
    where
      run :: TestEmulator s a -> ST s a
      run (TestEmulator reader) = do
        vm <- VM.new
        runReaderT reader vm
