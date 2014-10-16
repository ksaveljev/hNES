module NES.MemoryMap ( MemoryMap(..)
                     , new
                     , hasChrRam
                     ) where

import Data.Word (Word8, Word16)
import Data.Array.ST (STUArray, newArray, writeArray)
import Control.Monad (when)
import Control.Monad.ST (ST)
import qualified Data.ByteString as B

import NES.ROM (ROM(..))

-- We moved out the locations which are used in Mapper into MemoryMap
-- so that the mapper has easier access to the required fields
data MemoryMap s = MemoryMap { prgSize :: Word16
                             , chrSize :: Word16
                             , prg :: STUArray s Word16 Word8
                             , prgRAM :: STUArray s Word16 Word8
                             , chr :: STUArray s Word16 Word8
                             , ram :: STUArray s Word16 Word8
                             , nametables :: STUArray s Word16 Word8
                             , palette :: STUArray s Word8 Word8
                             }

new :: ROM -> ST s (MemoryMap s)
new catridge = do
    prg' <- newArray (0x0000, prgSize') 0
    loadPRG prg'
    prgRAM' <- newArray (0x0000, 0x2000) 0
    chr' <- if chrSize' == 0 then newArray (0x0000, 0x2000) 0 else newArray (0x0000, chrSize') 0
    when (chrSize' > 0) $ loadCHR chr'
    ram' <- newArray (0x0000, 0x0800) 0xFF
    nametables' <- newArray (0x0000, 0x1000) 0
    palette' <- newArray (0x00, 0x20) 0
    return MemoryMap { prgSize = prgSize'
                     , chrSize = chrSize'
                     , prg = prg'
                     , prgRAM = prgRAM'
                     , chr = chr'
                     , ram = ram'
                     , nametables = nametables'
                     , palette = palette'
                     }
    where
      prgSize' = fromIntegral $ B.length (prgROM catridge) :: Word16
      chrSize' = fromIntegral $ B.length (chrROM catridge) :: Word16
      loadByte storage index = writeArray storage index (B.index (prgROM catridge) (fromIntegral index))
      loadPRG storage = mapM_ (loadByte storage) [0..prgSize' - 1]
      loadCHR storage = mapM_ (loadByte storage) [0..chrSize' - 1]

hasChrRam :: MemoryMap s -> Bool
hasChrRam memoryMap = 0 == chrSize memoryMap
