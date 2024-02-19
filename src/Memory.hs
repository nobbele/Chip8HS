module Memory where

import Data.Vector.Unboxed (Vector, (!?))
import Data.Word
import Helper

readWord :: Word16 -> Vector Word8 -> Maybe Word16
readWord pc memory = mergeByte8 <$> aMaybe <*> bMaybe
  where
    aMaybe = memory !? fromIntegral pc
    bMaybe = memory !? (fromIntegral pc + 1)

readByte :: Word16 -> [Word8] -> Word8
readByte pc memory = memory !! fromIntegral pc