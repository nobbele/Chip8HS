module Helper where

import Data.Bits
import Data.Word

replaceNth :: Word8 -> (a -> a) -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal x : xs
  | otherwise = x : replaceNth (n - 1) newVal xs

mergeNibble :: Word8 -> Word8 -> Word8
mergeNibble a b = shiftL a 4 .|. b

mergeNibble3 :: Word8 -> Word8 -> Word8 -> Word16
mergeNibble3 a b c = shiftL (fromIntegral a) 8 .|. shiftL (fromIntegral b) 4 .|. fromIntegral c

mergeByte8 :: Word8 -> Word8 -> Word16
mergeByte8 a b = shiftL (fromIntegral a) 8 .|. fromIntegral b

mergeByte8L :: [Word8] -> Maybe Word16
mergeByte8L (a : b : _) = Just (shiftL (fromIntegral a) 8 .|. fromIntegral b)
mergeByte8L _ = Nothing

extractBytes16 :: Word16 -> (Word8, Word8)
extractBytes16 n = (fromIntegral (shiftR (n .&. 0xFF00) 0), fromIntegral (n .&. 0xFF))

extractNibbles8 :: Word8 -> (Word8, Word8)
extractNibbles8 n = (shiftR (n .&. 0xF0) 4, n .&. 0x0F)

extractNibbles16 :: Word16 -> (Word8, Word8, Word8, Word8)
extractNibbles16 n =
  ( fromIntegral (shiftR (n .&. 0xF000) 12),
    fromIntegral (shiftR (n .&. 0xF00) 8),
    fromIntegral (shiftR (n .&. 0xF0) 4),
    fromIntegral (n .&. 0x0F)
  )