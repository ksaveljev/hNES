module NES.CPU ( Flag(..)
               , CPU(..)
               , new
               , getFlag
               , setFlag
               , getCpuCycles
               , setCpuCycles
               ) where

import Data.Word (Word8, Word16, Word64)
import Data.Bits (testBit, setBit, clearBit)
import Data.STRef (STRef, readSTRef, writeSTRef, newSTRef)
import Control.Monad.ST (ST)

-- http://wiki.nesdev.com/w/index.php/Status_flags
--
-- CF: carry flag
-- ZF: zero flag
-- IDF: interrupt disable flag
-- DMF: decimal mode flag
-- BCF: break command flag (always set)
-- BF: "b" flag
-- OF: overflow flag
-- NF: negative flag
data Flag = CF | ZF | IDF | DMF | BF | BCF | OF | NF deriving (Enum, Show)

data CPU s = CPU { programCounter :: STRef s Word16
                 , stackPointer :: STRef s Word8
                 , registerA :: STRef s Word8
                 , registerX :: STRef s Word8
                 , registerY :: STRef s Word8
                 , cpuFlags :: STRef s Word8
                 , cpuCycles :: STRef s Word64
                 }

new :: ST s (CPU s)
new = do
    programCounter' <- newSTRef 0xC000
    stackPointer' <- newSTRef 0xFD
    registerA' <- newSTRef 0
    registerX' <- newSTRef 0
    registerY' <- newSTRef 0
    cpuFlags' <- newSTRef 0x24
    cpuCycles' <- newSTRef 0
    return CPU { programCounter = programCounter'
               , stackPointer   = stackPointer'
               , registerA      = registerA'
               , registerX      = registerX'
               , registerY      = registerY'
               , cpuFlags       = cpuFlags'
               , cpuCycles      = cpuCycles'
               }


getFlag :: CPU s -> Flag -> ST s Bool
getFlag cpu flag = readSTRef (cpuFlags cpu) >>= \status -> return $ testBit status (fromEnum flag)

setFlag :: CPU s -> Flag -> Bool -> ST s ()
setFlag cpu flag b = do
    status <- readSTRef (cpuFlags cpu)
    let updatedStatus = if b then setBit status (fromEnum flag)
                             else clearBit status (fromEnum flag)
    writeSTRef (cpuFlags cpu) updatedStatus

getCpuCycles :: CPU s -> ST s Word64
getCpuCycles = readSTRef . cpuCycles

setCpuCycles :: CPU s -> Word64 -> ST s ()
setCpuCycles = writeSTRef . cpuCycles
