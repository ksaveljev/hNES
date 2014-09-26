module NES.VM ( VM(..)
              ) where

import NES.CPU (CPU)

data VM s = VM { getCPU :: CPU s
               }
