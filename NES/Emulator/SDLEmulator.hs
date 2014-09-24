{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NES.Emulator.SDLEmulator where

import Control.Monad.Reader (ReaderT)
import Control.Monad.ST (RealWorld)
import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadIO)

import NES.VM (VM)

newtype SDLEmulator a = SDLEmulator (ReaderT (VM RealWorld) IO a)
                        deriving (Functor, Applicative, Monad, MonadIO)
