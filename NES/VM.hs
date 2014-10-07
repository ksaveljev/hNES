module NES.VM ( VM(..)
              , new
              ) where

import Control.Monad.ST (ST)
import Control.Applicative ((<$>))

import NES.CPU (CPU)
import qualified NES.CPU as CPU

data VM s = VM { cpu :: CPU s
               }

new :: ST s (VM s)
new = VM <$> CPU.new
