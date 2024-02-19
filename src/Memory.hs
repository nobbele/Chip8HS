module Memory where

import Data.Vector.Unboxed (Vector, (!?))
import Data.Word
import Helper

readWord :: Word16 -> Vector Word8 -> Maybe Word16
readWord pc memory = do
  a <- memory !? fromIntegral pc
  b <- memory !? (fromIntegral pc + 1)
  Just $ packBytes2 a b

readByte :: Word16 -> [Word8] -> Word8
readByte pc memory = memory !! fromIntegral pc