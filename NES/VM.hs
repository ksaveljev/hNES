module NES.VM ( VM(..)
              ) where

import Control.Monad.ST (ST)

import NES.CPU (CPU)
import qualified NES.CPU as CPU

data VM s = VM { getCPU :: CPU s
               }

new :: ST s (VM s)
new = do
    cpu <- CPU.new
    return $ VM cpu
