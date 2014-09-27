module NES.ROM where

import Data.Word (Word8)
import qualified Data.ByteString as B

data NESHeader = NESHeader { magic :: [Char]     -- "NES"<EOF>
                           , prgROMsize :: Word8 -- Number of 16384 byte program ROM pages
                           , chrROMsize :: Word8 -- Number of 8192 byte character ROM pages (0 indicates CHR RAM)
                           , flags6 :: Word8     -- Flags 6
                           , flags7 :: Word8     -- Flags 7
                           , zero :: [Word8]     -- These bytes are not used, and should be 0
                           }

data ROM = ROM { header :: NESHeader
               , prgROM :: B.ByteString
               , chrROM :: B.ByteString
               }
