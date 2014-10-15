module NES.PPU ( PPU(..)
               , new
               , isPPUOn
               , getVramAddrIncrement
               , loopyVXIncrement
               , loopyVYIncrement
               ) where

import Data.Word (Word8, Word16)
import Data.Bits (testBit, (.&.), (.|.), xor, complement, shiftR, shiftL)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Array.ST (STUArray, newArray)
import Control.Monad.ST (ST)

data PPU s = PPU { ppuCtrl :: STRef s Word8
                 , ppuMask :: STRef s Word8
                 , ppuStatus :: STRef s Word8
                 , oamAddr :: STRef s Word8
                 , oamData :: STRef s Word8
                 -- ppuScroll, ppuAddress and ppuData are emulated using
                 -- these values: loopyV, loopyT, loopyX, scrollLatch
                 -- refer to
                 -- http://forums.nesdev.com/viewtopic.php?t=664
                 -- http://wiki.nesdev.com/w/index.php/The_skinny_on_NES_scrolling
                 , loopyV :: STRef s Word16 -- PPU memory address
                 , loopyT :: STRef s Word16 -- temporary address
                 , loopyX :: STRef s Word8 -- fine horizontal scroll
                 , scrollLatch :: STRef s Bool
                 , bufferedValue :: STRef s Word8
                 , scanline :: STRef s Word16
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
    loopyV' <- newSTRef 0x0
    loopyT' <- newSTRef 0x0
    loopyX' <- newSTRef 0x0
    scrollLatch' <- newSTRef True
    bufferedValue' <- newSTRef 0x0
    scanline' <- newSTRef 0x0
    oam' <- newArray (0x00, 0xFF) 0xFF
    openBus' <- newSTRef 0x0
    return PPU { ppuCtrl = ppuCtrl'
               , ppuMask = ppuMask'
               , ppuStatus = ppuStatus'
               , oamAddr = oamAddr'
               , oamData = oamData'
               , loopyV = loopyV'
               , loopyT = loopyT'
               , loopyX = loopyX'
               , scrollLatch = scrollLatch'
               , bufferedValue = bufferedValue'
               , scanline = scanline'
               , oam = oam'
               , openBus = openBus'
               }

isPPUOn :: PPU s -> ST s Bool
isPPUOn ppu = do
    mask <- readSTRef (ppuMask ppu)
    return $ testBit mask 3 || testBit mask 4

getVramAddrIncrement :: PPU s -> ST s Word16
getVramAddrIncrement ppu = do
    ctrl <- readSTRef (ppuCtrl ppu)
    return $ if testBit ctrl 2 then 32 else 1

loopyVXIncrement :: PPU s -> ST s ()
loopyVXIncrement ppu = do
    v <- readSTRef (loopyV ppu)
    -- if coarse X == 31 then coarse X = 0 and switch horizontal nametable else increment coarse X
    writeSTRef (loopyV ppu) $ if v .&. 0x001F == 31 then (v .&. complement 0x001F) `xor` 0x0400 else v + 1

loopyVYIncrement :: PPU s -> ST s ()
loopyVYIncrement ppu = do
    v <- readSTRef (loopyV ppu)
    if v .&. 0x7000 == 0x7000
      then do
        let v' = v .&. complement 0x7000            -- fine Y = 0
            y = (v' .&. 0x03E0) `shiftR` 5          -- let y = coarse Y
            (y', v'') = case y of
                          29 -> (0, v' `xor` 0x0800) -- coarse Y = 0, switch vertical nametable
                          31 -> (0, v')              -- coarse Y = 0, nametable not switched
                          _ -> (y + 1, v')           -- increment coarse Y
        writeSTRef (loopyV ppu) $ (v'' .&. complement 0x03E0) .|. (y' `shiftL` 5) -- put coarse Y back
      else writeSTRef (loopyV ppu) (v + 0x1000) -- if fine Y < 7 then increment fine Y
