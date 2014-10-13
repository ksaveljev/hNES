module NES.PPU ( PPU(..)
               , new
               , readPPURegister
               , writePPURegister
               ) where

import Data.Word (Word8, Word16)
import Data.Bits ((.&.), (.|.))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Array.ST (STUArray, newArray, readArray)
import Control.Monad.ST (ST)

data PPU s = PPU { ppuCtrl :: STRef s Word8
                 , ppuMask :: STRef s Word8
                 , ppuStatus :: STRef s Word8
                 , oamAddr :: STRef s Word8
                 , oamData :: STRef s Word8
                 , ppuScroll :: STRef s Word8
                 , ppuAddress :: STRef s Word8
                 , ppuData :: STRef s Word8
                 , oam :: STUArray s Word8 Word8 -- sprite table (up to 64 sprites, 4 bytes per sprite)
                 , openBus :: STRef s Word8 -- open bus for reading write-only and writing read-only registers
                 }

new :: ST s (PPU s)
new = do
    ppuCtrl' <- newSTRef 0x0
    ppuMask' <- newSTRef 0x0
    ppuStatus' <- newSTRef 0x0
    oamAddr' <- newSTRef 0x0
    oamData' <- newSTRef 0x0
    ppuScroll' <- newSTRef 0x0
    ppuAddress' <- newSTRef 0x0
    ppuData' <- newSTRef 0x0
    oam' <- newArray (0x00, 0xFF) 0xFF
    openBus' <- newSTRef 0x0
    return PPU { ppuCtrl = ppuCtrl'
               , ppuMask = ppuMask'
               , ppuStatus = ppuStatus'
               , oamAddr = oamAddr'
               , oamData = oamData'
               , ppuScroll = ppuScroll'
               , ppuAddress = ppuAddress'
               , ppuData = ppuData'
               , oam = oam'
               , openBus = openBus'
               }

readPPURegister :: PPU s -> Word16 -> ST s Word8
readPPURegister ppu addr =
    case addr .&. 7 of
      2 -> do
        w8 <- readSTRef (ppuStatus ppu)
        bus <- readSTRef (openBus ppu)
        let tmp = w8 .|. (bus .&. 0x1F)
            updatedBus = (bus .&. 0x1F) .|. (w8 .&. 0xE0)
        writeSTRef (openBus ppu) updatedBus
        return tmp
      4 -> readSTRef (oamAddr ppu) >>= readArray (oam ppu) >>= \w8 -> writeSTRef (openBus ppu) w8 >> return w8
      7 -> undefined
      _ -> readSTRef (openBus ppu)


writePPURegister :: PPU s -> Word16 -> Word8 -> ST s ()
writePPURegister ppu addr w8 =
    case addr .&. 7 of
      0 -> undefined
      1 -> undefined
      2 -> undefined
      3 -> undefined
      4 -> undefined
      5 -> undefined
      6 -> undefined
      7 -> undefined
      _ -> error "cannot happen"
