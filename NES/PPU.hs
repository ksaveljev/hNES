module NES.PPU ( PPU(..)
               , new
               ) where

import Data.Word (Word8, Word16)
import Data.STRef (STRef, newSTRef)
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
               , oam = oam'
               , openBus = openBus'
               }
