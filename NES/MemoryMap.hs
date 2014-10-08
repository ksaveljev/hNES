module NES.MemoryMap ( MemoryMap(..)
                     , new
                     ) where

import Data.Word (Word8, Word16)
import Data.Array.ST (STUArray)
import Control.Monad.ST (ST)

data MemoryMap s = MemoryMap { ram :: STUArray s Word16 Word8
                             , prgRAM :: STUArray s Word16 Word8
                             , vram :: STUArray s Word16 Word8
                             }

new :: ST s (MemoryMap s)
new = undefined
