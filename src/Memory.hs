module Memory where

import Data.Word
import Helper

readWord :: Word16 -> [Word8] -> Maybe Word16
readWord pc memory = mergeByte8L (take 2 (drop (fromIntegral pc) memory))

readByte :: Word16 -> [Word8] -> Word8
readByte pc memory = memory !! fromIntegral pc