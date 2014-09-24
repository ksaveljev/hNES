module NES.CPU ( MemoryValue
               , Address
               , CPU
               ) where

import Data.Word (Word8, Word16, Word64)
import Data.STRef (STRef)
import Data.Array.ST (STUArray)

data Register = Pc | Sp | A | X | Y

data Flags = CF | ZF | IDF | DMF | BCF | OF | NF deriving (Enum)

data MemoryValue = Mem8 Word8
                 | Mem16 Word16

data Address = Register Register
             | Ram Word16

data CPU s = CPU { cpuMemory :: STUArray s Word16 Word8
                 , cpuRegisters :: STUArray s Word8 Word8
                 , cpuFlags :: STRef s Word8
                 , cpuCycles :: STRef s Word64
                 }

