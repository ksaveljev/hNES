module NES.Mapper where

import Data.Word (Word8, Word16)

data Mapper = Mapper { prgLoad :: Word16 -> Word8
                     , prgStore :: Word16 -> Word8 -> ()
                     , chrLoad :: Word16 -> Word8
                     , chrStore :: Word16 -> Word8 -> ()
                     }

loadMapper :: Word8 -> Mapper
loadMapper n =
    case n of
      -- Mapper 0: NROM
      0 -> undefined
      _ -> error "unsupported Mapper"
