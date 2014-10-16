module NES.Mapper where

import Data.Word (Word8, Word16)
import Data.Bits ((.&.), shiftR, shiftL, testBit)
import Data.STRef (STRef, newSTRef, readSTRef)
import Data.Array.ST (readArray, writeArray)
import Control.Monad.ST (ST)
import Control.Monad (when)

import NES.ROM
import NES.MemoryMap

data NameTable = NT0 | NT1 | NT2 | NT3

data NameTableMap = NameTableMap { nt0 :: NameTable
                                 , nt1 :: NameTable
                                 , nt2 :: NameTable
                                 , nt3 :: NameTable
                                 }

nameTableAddress :: NameTable -> Word16
nameTableAddress nametable = case nametable of
                               NT0 -> 0x0000
                               NT1 -> 0x0400
                               NT2 -> 0x0800
                               NT3 -> 0x0C00

data MirrorType = HorizontalMirror
                | VerticalMirror
                | SingleScreenMirror0
                | SingleScreenMirror1
                | FourScreenMirror


data Mapper s = Mapper0 (STRef s NameTableMap)

prgLoad :: Mapper s -> MemoryMap s -> Word16 -> ST s Word8
prgLoad (Mapper0 _) mem addr = 
    case addr of
      x | x >= 0x8000 -> readArray (prg mem) (addr .&. (if prgSize mem > 0x4000 then 0x7FFF else 0x3FFF))
      x | x >= 0x6000 -> readArray (prgRAM mem) (addr .&. 0x1FFF)
      _ -> return 0

prgStore :: Mapper s -> MemoryMap s -> Word16 -> Word8 -> ST s ()
prgStore (Mapper0 _) mem addr w8 =
    when (addr >= 0x6000 && addr < 0x8000) $ writeArray (prgRAM mem) (addr .&. 0x1FFF) w8

chrLoad :: Mapper s -> MemoryMap s -> Word16 -> ST s Word8
chrLoad (Mapper0 ntMap) mem addr =
    if addr < 0x2000
      then readArray (chr mem) addr
      else do
        ntMap' <- readSTRef ntMap
        case addr .&. 0xC00 of
          0x000 -> readArray (nametables mem) (addr .&. 0x3FF + nameTableAddress (nt0 ntMap'))
          0x400 -> readArray (nametables mem) (addr .&. 0x3FF + nameTableAddress (nt1 ntMap'))
          0x800 -> readArray (nametables mem) (addr .&. 0x3FF + nameTableAddress (nt2 ntMap'))
          _ -> if addr >= 0x3F00 -- includes addr .&. 0xC00 == 0xC00
                 then do
                   let addr' = fromIntegral $ addr .&. 0x1F
                       addr'' = if addr' >= 0x10 && (addr' .&. 0x3 == 0) then addr' - 0x10 else addr'
                   readArray (palette mem) addr''
                 else readArray (nametables mem) (addr .&. 0x3FF + nameTableAddress (nt3 ntMap'))

chrStore :: Mapper s -> MemoryMap s -> Word16 -> Word8 -> ST s ()
chrStore (Mapper0 ntMap) mem addr w8 = do
    let addr' = addr .&. 0x3FFF
    if addr' < 0x2000
      then when (hasChrRam mem) $ writeArray (chr mem) addr' w8
      else do
        ntMap' <- readSTRef ntMap
        case addr' .&. 0xC00 of
          0x000 -> writeArray (nametables mem) (addr' .&. 0x3FF + nameTableAddress (nt0 ntMap')) w8
          0x400 -> writeArray (nametables mem) (addr' .&. 0x3FF + nameTableAddress (nt1 ntMap')) w8
          0x800 -> writeArray (nametables mem) (addr' .&. 0x3FF + nameTableAddress (nt2 ntMap')) w8
          0xC00 -> if addr' >= 0x3F00 && addr' <= 0x3FFF
                 then do
                   let addr'' = fromIntegral $ addr' .&. 0x1F
                       addr''' = if addr'' >= 0x10 && (addr'' .&. 0x3 == 0) then addr'' - 0x10 else addr''
                   writeArray (palette mem) addr''' (w8 .&. 0x3F)
                 else writeArray (nametables mem) (addr' .&. 0x3FF + nameTableAddress (nt3 ntMap')) w8
          _ -> return ()


loadMapper :: ROM -> ST s (Mapper s)
loadMapper catridge =
    case mapnum of
      0 -> do
        ntMap <- newSTRef defaultNameTableMap
        return $ Mapper0 ntMap
      x -> error $ "Mapper " ++ show x ++ " not implemented"
    where
      mapnum = (flags6 (header catridge) `shiftR` 4) + ((flags7 (header catridge) `shiftR` 4) `shiftL` 4)
      defaultNameTableMap
        | testBit (flags6 $ header catridge) 3 = setMirroring FourScreenMirror
        | testBit (flags6 $ header catridge) 0 = setMirroring VerticalMirror
        | otherwise = setMirroring HorizontalMirror

setMirroring :: MirrorType -> NameTableMap
setMirroring mirrorType =
    case mirrorType of
      HorizontalMirror -> NameTableMap NT0 NT0 NT1 NT1
      VerticalMirror -> NameTableMap NT0 NT1 NT0 NT1
      FourScreenMirror -> NameTableMap NT0 NT1 NT2 NT3
      SingleScreenMirror0 -> NameTableMap NT0 NT0 NT0 NT0
      SingleScreenMirror1 -> NameTableMap NT1 NT1 NT1 NT1
