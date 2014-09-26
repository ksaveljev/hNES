module NES.CPU ( Flag(..)
               , Storage(..)
               , CPU(..)
               ) where

import Data.Word (Word8, Word16, Word64)
import Data.STRef (STRef)
import Data.Array.ST (STUArray)

-- Pc: program counter
-- Sp: stack pointer
-- A: accumulator
-- X: index register X
-- Y: index register Y
-- SR: processor status register
-- Ram: location in memory
data Storage = Pc | Sp | A | X | Y | SR | Ram Word16

-- CF: carry flag
-- ZF: zero flag
-- IDF: interrupt disable flag
-- DMF: decimal mode flag
-- BCF: break command flag
-- OF: overflow flag
-- NF: negative flag
data Flag = CF | ZF | IDF | DMF | BCF | OF | NF deriving (Enum)

data CPU s = CPU { cpuMemory :: STUArray s Word16 Word8
                 , programCounter :: STRef s Word8
                 , stackPointer :: STRef s Word8
                 , registerA :: STRef s Word8
                 , registerX :: STRef s Word8
                 , registerY :: STRef s Word8
                 , cpuFlags :: STRef s Word8
                 , cpuCycles :: STRef s Word64
                 }

