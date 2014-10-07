module NES.ROM where

import Data.Word (Word8)
import qualified Data.ByteString as B

{-
iNES file format: http://wiki.nesdev.com/w/index.php/INES

An iNES file consists of the following sections, in order:
    1. Header (16 bytes)
    2. Trainer, if present (0 or 512 bytes)
    3. PRG ROM data (16384 * x bytes)
    4. CHR ROM data, if present (8192 * y bytes)
    5. <currently not interested>
    6. <currently not interested>
-}
data ROM = ROM { header :: NESHeader
               -- TODO: trainer 0 or 512 bytes
               , prgROM :: B.ByteString
               , chrROM :: B.ByteString
               }

data NESHeader = NESHeader { magic :: B.ByteString -- "NES"<EOF>
                           , prgROMsize :: Word8   -- Number of 16384 byte program ROM pages
                           , chrROMsize :: Word8   -- Number of 8192 byte character ROM pages (0 indicates CHR RAM)
                           , flags6 :: Word8       -- Flags 6
                           , flags7 :: Word8       -- Flags 7
                           , prgRAMsize :: Word8   -- Size of PRG RAM in 8 KB units (Value 0 infers 8 KB for compatibility)
                           , flags9 :: Word8       -- Flags 9
                           , flags10 :: Word8      -- Flags 10
                           , zeros :: B.ByteString -- These bytes are not used, and should be 0
                           }

loadROM :: B.ByteString -> ROM
loadROM bytes = ROM h prg chr
  where
    h = parseHeader $ B.take 16 bytes
    prgSize = 16384 * fromIntegral (prgROMsize h)
    prg = B.take prgSize . B.drop 16 $ bytes
    chrSize = 8192 * fromIntegral (chrROMsize h)
    chr = B.take chrSize . B.drop (16 + prgSize) $ bytes

parseHeader :: B.ByteString -> NESHeader
parseHeader bytes = NESHeader { magic      = B.take 4 bytes
                              , prgROMsize = B.index bytes 4
                              , chrROMsize = B.index bytes 5
                              , flags6     = B.index bytes 6
                              , flags7     = B.index bytes 7
                              , prgRAMsize = B.index bytes 8
                              , flags9     = B.index bytes 9
                              , flags10    = B.index bytes 10
                              , zeros      = B.drop 11 bytes
                              }
