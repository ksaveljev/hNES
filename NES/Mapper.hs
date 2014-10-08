module NES.Mapper where

import Data.Word (Word8, Word16)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Array.ST (readArray, writeArray)
import Control.Monad.ST (ST)
import Control.Monad (when)

import NES.ROM
import NES.MemoryMap (MemoryMap(..))

data Mapper s = Mapper0

prgLoad :: Mapper s -> MemoryMap s -> Word16 -> ST s Word8
prgLoad Mapper0 mem addr = 
    case addr of
      x | x >= 0x8000 -> readArray (prg mem) (addr .&. (if prgSize mem > 0x4000 then 0x7FFF else 0x3FFF))
      x | x >= 0x6000 -> readArray (prgRAM mem) (addr .&. 0x1FFF)
      _ -> return 0

prgStore :: Mapper s -> MemoryMap s -> Word16 -> Word8 -> ST s ()
prgStore Mapper0 mem addr w8 =
    when (addr >= 0x6000 && addr < 0x8000) $ writeArray (prgRAM mem) (addr .&. 0x1FFF) w8

chrLoad :: Mapper s -> MemoryMap s -> Word16 -> ST s Word8
chrLoad = undefined

chrStore :: Mapper s -> MemoryMap s -> Word16 -> Word8 -> ST s ()
chrStore = undefined


loadMapper :: ROM -> Mapper s
loadMapper catridge =
    case mapnum of
      0 -> Mapper0
      x -> error $ "Mapper " ++ show x ++ " not implemented"
    where
      mapnum = (flags6 (header catridge) `shiftR` 4) + ((flags7 (header catridge) `shiftR` 4) `shiftL` 4)

