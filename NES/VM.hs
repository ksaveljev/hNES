module NES.VM ( VM
              ) where

import Data.STRef (STRef)

import NES.CPU (CPU)

data VM s = VM { cpu :: STRef s (CPU s)
               }
