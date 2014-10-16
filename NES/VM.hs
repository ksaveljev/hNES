{-# LANGUAGE Rank2Types #-}
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
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Control.Monad.ST (ST)
import Data.Array.ST (readArray, writeArray)
import Control.Applicative ((<$>), (<*>))

import NES.ROM (ROM)
import NES.Mapper (Mapper, loadMapper, prgLoad, prgStore, chrLoad, chrStore)
import NES.MemoryMap (MemoryMap(..))
import NES.CPU (CPU(..))
import NES.PPU hiding (new)
import qualified NES.CPU as CPU
import qualified NES.PPU as PPU
import qualified NES.MemoryMap as MemoryMap

data VM s = VM { rom :: ROM
               , mapper :: Mapper s
               , memoryMap :: MemoryMap s
               , cpu :: CPU s
               , ppu :: PPU s
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
new catridge = do
    mapper' <- loadMapper catridge
    VM catridge mapper' <$> MemoryMap.new catridge <*> CPU.new <*> PPU.new

load8 :: VM s -> Storage -> ST s Word8
load8 vm Sp          = readSTRef (stackPointer $ cpu vm)
load8 vm A           = readSTRef (registerA $ cpu vm)
load8 vm X           = readSTRef (registerX $ cpu vm)
load8 vm Y           = readSTRef (registerY $ cpu vm)
load8 vm SR          = readSTRef (cpuFlags $ cpu vm)
load8 vm (Ram addr)  | addr < 0x2000 = readArray (ram $ memoryMap vm) (addr .&. 0x7FF)
                     | addr < 0x4000 = readPPURegister vm addr
                     | otherwise = prgLoad (mapper vm) (memoryMap vm) addr
load8 vm (VRam addr) = seq (vm, addr) undefined -- TODO
load8 _  Pc          = error "Trying to load word8 from word16 register (PC)"

store8 :: VM s -> Storage -> Word8 -> ST s ()
store8 vm Sp          w8 = writeSTRef (stackPointer $ cpu vm) w8
store8 vm A           w8 = writeSTRef (registerA $ cpu vm) w8
store8 vm X           w8 = writeSTRef (registerX $ cpu vm) w8 
store8 vm Y           w8 = writeSTRef (registerY $ cpu vm) w8
store8 vm SR          w8 = writeSTRef (cpuFlags $ cpu vm) w8
store8 vm (Ram addr)  w8 | addr < 0x2000 = writeArray (ram $ memoryMap vm) (addr .&. 0x7FF) w8
                         | addr < 0x4000 = writePPURegister vm addr w8
                         | otherwise = prgStore (mapper vm) (memoryMap vm) addr w8
store8 vm (VRam addr) w8 = seq (vm, addr, w8) undefined -- TODO
store8 _  Pc          _  = error "Trying to store word8 to word16 register (PC)"

load16 :: VM s -> Storage -> ST s Word16
load16 vm Pc = readSTRef (programCounter $ cpu vm)
load16 _  _  = error "Trying to load word16 other than from PC location"

store16 :: VM s -> Storage -> Word16 -> ST s ()
store16 vm Pc w16 = writeSTRef (programCounter $ cpu vm) w16
store16 _  _  _   = error "Trying to store word16 to other than PC location"

readPPURegister :: VM s -> Word16 -> ST s Word8
readPPURegister vm addr =
    case addr .&. 7 of
      2 -> do
        w8 <- readSTRef (ppuStatus $ ppu vm)
        bus <- readSTRef (openBus $ ppu vm)
        let tmp = w8 .|. (bus .&. 0x1F)
            updatedBus = (bus .&. 0x1F) .|. (w8 .&. 0xE0)
        writeSTRef (openBus $ ppu vm) updatedBus
        return tmp
      4 -> do
        oamAddress <- readSTRef (oamAddr $ ppu vm)
        w8 <- readArray (oam $ ppu vm) oamAddress
        writeSTRef (openBus $ ppu vm) w8
        return w8
      7 -> do -- Reads are delayed by one cycle (except if address is in palette range)
        loopyV' <- readSTRef (loopyV $ ppu vm)
        w8 <- if loopyV' < 0x3F00 then do
                                    buf <- readSTRef (bufferedValue $ ppu vm)
                                    result <- chrLoad (mapper vm) (memoryMap vm) loopyV'
                                    writeSTRef (bufferedValue $ ppu vm) result
                                    writeSTRef (openBus $ ppu vm) buf
                                    return buf
                                  else do
                                    buf <- chrLoad (mapper vm) (memoryMap vm) (loopyV' - 0x1000)
                                    writeSTRef (bufferedValue $ ppu vm) buf
                                    result <- chrLoad (mapper vm) (memoryMap vm) loopyV'
                                    writeSTRef (openBus $ ppu vm) result
                                    return result
        scanline' <- readSTRef (scanline $ ppu vm)
        ppuOn <- isPPUOn $ ppu vm
        if not ppuOn || (scanline' > 240 && scanline' < 261)
          then do
            incr <- getVramAddrIncrement $ ppu vm
            writeSTRef (loopyV $ ppu vm) (loopyV' + incr)
          else do
            -- if $2007 is read during the rendering then PPU increment
            -- both horizontal and vertical counters erroneously
            loopyVXIncrement (ppu vm)
            loopyVYIncrement (ppu vm)
        return w8
      _ -> readSTRef (openBus $ ppu vm)

writePPURegister :: VM s -> Word16 -> Word8 -> ST s ()
writePPURegister vm addr w8 =
    case addr .&. 7 of
      0 -> do
        writeSTRef (ppuCtrl $ ppu vm) w8
        writeSTRef (openBus $ ppu vm) w8
        loopyT' <- readSTRef (loopyT $ ppu vm)
        writeSTRef (loopyT $ ppu vm) $ (loopyT' .&. 0x73FF) + ((fromIntegral w8 .&. 0x3) `shiftL` 10)
      1 -> do
        writeSTRef (ppuMask $ ppu vm) w8
        writeSTRef (openBus $ ppu vm) w8
      2 -> return () -- read-only register, writing to it does nothing
      3 -> do
        writeSTRef (oamAddr $ ppu vm) w8
        writeSTRef (openBus $ ppu vm) w8
      4 -> do
        oamAddress <- readSTRef (oamAddr $ ppu vm)
        writeArray (oam $ ppu vm) oamAddress (w8 .&. 0xE3)
        writeSTRef (oamAddr $ ppu vm) (oamAddress + 1)
        writeSTRef (openBus $ ppu vm) w8
      5 -> do
        latch <- readSTRef (scrollLatch $ ppu vm)
        loopyT' <- readSTRef (loopyT $ ppu vm)
        case latch of
          True -> do
            let tValue = loopyT' .&. 0x7FE0
            writeSTRef (loopyT $ ppu vm) $ tValue + fromIntegral (w8 `shiftR` 3)
            writeSTRef (loopyX $ ppu vm) (w8 .&. 0x7)
          False -> do
            let tValue = ((fromIntegral w8 .&. 0xF8) `shiftL` 2) .|. ((fromIntegral w8 .&. 0x7) `shiftL` 12)
            writeSTRef (loopyT $ ppu vm) $ (loopyT' .&. 0xC1F) .|. tValue
        writeSTRef (scrollLatch $ ppu vm) (not latch)
        writeSTRef (openBus $ ppu vm) w8
      6 -> do
        latch <- readSTRef (scrollLatch $ ppu vm)
        loopyT' <- readSTRef (loopyT $ ppu vm)
        case latch of
          True -> writeSTRef (loopyT $ ppu vm) $ (loopyT' .&. 0xFF) .|. ((fromIntegral w8 .&. 0x3F) `shiftL` 8)
          False -> do
            let tValue = (loopyT' .&. 0x7F00) .|. fromIntegral w8
            writeSTRef (loopyT $ ppu vm) tValue
            writeSTRef (loopyV $ ppu vm) tValue
        writeSTRef (scrollLatch $ ppu vm) (not latch)
        writeSTRef (openBus $ ppu vm) w8
      7 -> do
        scanline' <- readSTRef (scanline $ ppu vm)
        loopyV' <- readSTRef (loopyV $ ppu vm)
        chrStore (mapper vm) (memoryMap vm) loopyV' w8
        ppuOn <- isPPUOn $ ppu vm
        if not ppuOn || (scanline' > 240 && scanline' < 261)
          then do
            incr <- getVramAddrIncrement $ ppu vm
            writeSTRef (loopyV $ ppu vm) (loopyV' + incr)
          else undefined -- TODO: not sure about this case... something about incorrect loopyVXIncrement and loopyVYIncrement
      _ -> error "cannot happen"
