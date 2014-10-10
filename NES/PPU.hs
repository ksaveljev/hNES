module NES.PPU ( PPU(..)
               , new
               ) where

import Data.Word (Word8)
import Data.STRef (STRef, newSTRef)
import Data.Array.ST (STUArray, newArray)
import Control.Monad.ST (ST)

data PPU s = PPU { ppuCtrl :: STRef s Word8
                 , ppuMask :: STRef s Word8
                 , ppuStatus :: STRef s Word8
                 , oamAddr :: STRef s Word8
                 , oamData :: STRef s Word8
                 , ppuScroll :: STRef s Word8
                 , ppuAddress :: STRef s Word8
                 , ppuData :: STRef s Word8
                 , oam :: STUArray s Word8 Word8
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
    return PPU { ppuCtrl = ppuCtrl'
               , ppuMask = ppuMask'
               , ppuStatus = ppuStatus'
               , oamAddr = oamAddr'
               , oamData = oamData'
               , ppuScroll = ppuScroll'
               , ppuAddress = ppuAddress'
               , ppuData = ppuData'
               , oam = oam'
               }
