module NES.VM ( VM(..)
              , new
              ) where

import Control.Monad.ST (ST)
import Control.Applicative ((<$>))

import NES.CPU (CPU)
import NES.ROM (ROM)
import qualified NES.CPU as CPU

data VM s = VM { rom :: ROM
               , cpu :: CPU s
               --, ppu :: PPU s
               --, mapper :: Mapper
               }

new :: ROM -> ST s (VM s)
new catridge = VM catridge <$> CPU.new
