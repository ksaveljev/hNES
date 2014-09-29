module NES.ROM where

import Data.Word (Word8)
import qualified Data.ByteString as B

data NESHeader = NESHeader { magic :: B.ByteString -- "NES"<EOF>
                           , prgROMsize :: Word8   -- Number of 16384 byte program ROM pages
                           , chrROMsize :: Word8   -- Number of 8192 byte character ROM pages (0 indicates CHR RAM)
                           , flags6 :: Word8       -- Flags 6
                           , flags7 :: Word8       -- Flags 7
                           , zero :: B.ByteString  -- These bytes are not used, and should be 0
                           }

data ROM = ROM { header :: NESHeader
               , prgROM :: B.ByteString
               , chrROM :: B.ByteString
               }

loadROM :: B.ByteString -> ROM
loadROM bytes = ROM h prg chr
  where
    h = parseHeader $ B.take 15 bytes
    prg = undefined
    chr = undefined

parseHeader :: B.ByteString -> NESHeader
parseHeader bytes = NESHeader { magic = B.take 4 bytes
                              , prgROMsize = B.index bytes 4
                              , chrROMsize = B.index bytes 5
                              , flags6 = B.index bytes 6
                              , flags7 = B.index bytes 7
                              , zero = B.drop 8 bytes
                              }
