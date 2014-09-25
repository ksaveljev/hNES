module NES.Util where

import Data.Int (Int8)
import Data.Word (Word8, Word16)
import Data.Bits ((.|.), (.&.), shiftL, xor)

makeW16 :: Word8 -> Word8 -> Word16
makeW16 h l = ((fromIntegral h :: Word16) `shiftL` 8) .|. (fromIntegral l :: Word16)

bToW8 :: Bool -> Word8
bToW8 flag = if flag then 1 else 0

-- http://forums.nesdev.com/viewtopic.php?p=60520
isOverflow :: Word8 -> Word8 -> Word8 -> Bool
isOverflow a b result = (a `xor` result) .&. (b `xor` result) .&. 0x80 /= 0

makeSigned :: Word8 -> Int8
makeSigned = fromIntegral
