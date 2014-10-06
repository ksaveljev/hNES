module NES.CPU ( Flag(..)
               , Storage(..)
               , CPU(..)
               , new
               , load8
               , store8
               , load16
               , store16
               , getFlag
               , setFlag
               , getCpuCycles
               , setCpuCycles
               ) where

import Data.Word (Word8, Word16, Word64)
import Data.Bits (shiftR, testBit, setBit, clearBit)
import Data.STRef (STRef, readSTRef, writeSTRef, newSTRef)
import Data.Array.ST (STUArray, readArray, writeArray, newArray)
import Control.Monad.ST (ST)

import NES.Util

-- Pc: program counter
-- Sp: stack pointer
-- A: accumulator
-- X: index register X
-- Y: index register Y
-- SR: processor status register
-- Ram: location in memory
data Storage = Pc | Sp | A | X | Y | SR | Ram Word16 deriving Show

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

data CPU s = CPU { cpuMemory :: STUArray s Word16 Word8
                 , programCounter :: STRef s Word16
                 , stackPointer :: STRef s Word8
                 , registerA :: STRef s Word8
                 , registerX :: STRef s Word8
                 , registerY :: STRef s Word8
                 , cpuFlags :: STRef s Word8
                 , cpuCycles :: STRef s Word64
                 }

new :: ST s (CPU s)
new = do
    cpuMemory' <- newArray (0x0000, 0xFFFF) 0
    programCounter' <- newSTRef 0xC000
    stackPointer' <- newSTRef 0xFD
    registerA' <- newSTRef 0
    registerX' <- newSTRef 0
    registerY' <- newSTRef 0
    cpuFlags' <- newSTRef 0x24
    cpuCycles' <- newSTRef 0
    return CPU { cpuMemory = cpuMemory'
               , programCounter = programCounter'
               , stackPointer = stackPointer'
               , registerA = registerA'
               , registerX = registerX'
               , registerY = registerY'
               , cpuFlags = cpuFlags'
               , cpuCycles = cpuCycles'
               }

load8 :: CPU s -> Storage -> ST s Word8
load8 cpu Sp         = readSTRef (stackPointer cpu)
load8 cpu A          = readSTRef (registerA cpu)
load8 cpu X          = readSTRef (registerX cpu)
load8 cpu Y          = readSTRef (registerY cpu)
load8 cpu SR         = readSTRef (cpuFlags cpu)
load8 cpu (Ram addr) = readArray (cpuMemory cpu) addr
load8 _   Pc         = error "Trying to load word8 from word16 register (PC)"

store8 :: CPU s -> Storage -> Word8 -> ST s ()
store8 cpu Sp         w8 = writeSTRef (stackPointer cpu) w8
store8 cpu A          w8 = writeSTRef (registerA cpu) w8
store8 cpu X          w8 = writeSTRef (registerX cpu) w8 
store8 cpu Y          w8 = writeSTRef (registerY cpu) w8
store8 cpu SR         w8 = writeSTRef (cpuFlags cpu) w8
store8 cpu (Ram addr) w8 = writeArray (cpuMemory cpu) addr w8
store8 _   Pc         _  = error "Trying to store word8 to word16 register (PC)"

load16 :: CPU s -> Storage -> ST s Word16
load16 cpu Pc          = readSTRef (programCounter cpu)
load16 _   (Ram 65535) = error "Trying to read word16 from Ram 65535"
load16 cpu (Ram addr)  = do low <- readArray (cpuMemory cpu) addr
                            high <- readArray (cpuMemory cpu) (addr + 1)
                            return $ makeW16 high low
load16 _   _           = error "Trying to load word16 from word8 register"

store16 :: CPU s -> Storage -> Word16 -> ST s ()
store16 cpu Pc          w16 = writeSTRef (programCounter cpu) w16
store16 _   (Ram 65535) _   = error "Trying to write word16 to Ram 65535"
store16 cpu (Ram addr)  w16 = do let low = fromIntegral (w16 `shiftR` 8) :: Word8
                                 let high = fromIntegral w16 :: Word8
                                 writeArray (cpuMemory cpu) addr low
                                 writeArray (cpuMemory cpu) (addr + 1) high
store16 _   _           _   = error "Trying to store word16 to word8 register"

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
