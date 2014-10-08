module NES.VM ( VM(..)
              , new
              , Storage(..)
              , load8
              , store8
              , load16
              , store16
              ) where

import Data.Word (Word8, Word16)
import Data.STRef (readSTRef, writeSTRef)
import Data.Bits (shiftR)
import Control.Monad.ST (ST)
import Data.Array.ST (readArray, writeArray)
import Control.Applicative ((<$>), (<*>))

import NES.ROM (ROM)
import NES.MemoryMap (MemoryMap(..))
import NES.CPU (CPU(..))
import NES.Util
import qualified NES.CPU as CPU
import qualified NES.MemoryMap as MemoryMap

data VM s = VM { rom :: ROM
               , memoryMap :: MemoryMap s
               , cpu :: CPU s
               --, ppu :: PPU s
               --, mapper :: Mapper
               }

-- Pc: program counter
-- Sp: stack pointer
-- A: accumulator
-- X: index register X
-- Y: index register Y
-- SR: processor status register
-- Ram: location in memory
data Storage = Pc | Sp | A | X | Y | SR | Ram Word16 | VRam Word16 deriving Show

new :: ROM -> ST s (VM s)
new catridge = VM catridge <$> MemoryMap.new <*> CPU.new

load8 :: VM s -> Storage -> ST s Word8
load8 vm Sp          = readSTRef (stackPointer $ cpu vm)
load8 vm A           = readSTRef (registerA $ cpu vm)
load8 vm X           = readSTRef (registerX $ cpu vm)
load8 vm Y           = readSTRef (registerY $ cpu vm)
load8 vm SR          = readSTRef (cpuFlags $ cpu vm)
load8 vm (Ram addr)  = readArray (ram $ memoryMap vm) addr
--load8 vm (VRam addr) = undefined -- TODO
load8 _  Pc          = error "Trying to load word8 from word16 register (PC)"

store8 :: VM s -> Storage -> Word8 -> ST s ()
store8 vm Sp          w8 = writeSTRef (stackPointer $ cpu vm) w8
store8 vm A           w8 = writeSTRef (registerA $ cpu vm) w8
store8 vm X           w8 = writeSTRef (registerX $ cpu vm) w8 
store8 vm Y           w8 = writeSTRef (registerY $ cpu vm) w8
store8 vm SR          w8 = writeSTRef (cpuFlags $ cpu vm) w8
store8 vm (Ram addr)  w8 = writeArray (ram $ memoryMap vm) addr w8
--store8 vm (VRam addr) w8 = undefined -- TODO
store8 _  Pc          _  = error "Trying to store word8 to word16 register (PC)"

load16 :: VM s -> Storage -> ST s Word16
load16 vm Pc          = readSTRef (programCounter $ cpu vm)
load16 _  (Ram 65535) = error "Trying to read word16 from Ram 65535"
load16 vm (Ram addr)  = do low <- readArray (ram $ memoryMap vm) addr
                           high <- readArray (ram $ memoryMap vm) (addr + 1)
                           return $ makeW16 high low
load16 _  _           = error "Trying to load word16 from word8 register"

store16 :: VM s -> Storage -> Word16 -> ST s ()
store16 vm Pc          w16 = writeSTRef (programCounter $ cpu vm) w16
store16 _  (Ram 65535) _   = error "Trying to write word16 to Ram 65535"
store16 vm (Ram addr)  w16 = do let low = fromIntegral (w16 `shiftR` 8) :: Word8
                                let high = fromIntegral w16 :: Word8
                                writeArray (ram $ memoryMap vm) addr low
                                writeArray (ram $ memoryMap vm) (addr + 1) high
store16 _  _           _   = error "Trying to store word16 to word8 register"
