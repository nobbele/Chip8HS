module Helper where

import Data.Bits
import Data.Word

packNibbles2 :: Word8 -> Word8 -> Word8
packNibbles2 a b = shiftL a 4 .|. b

packNibbles3 :: Word8 -> Word8 -> Word8 -> Word16
packNibbles3 a b c = shiftL (fromIntegral a) 8 .|. shiftL (fromIntegral b) 4 .|. fromIntegral c

packBytes2 :: Word8 -> Word8 -> Word16
packBytes2 a b = shiftL (fromIntegral a) 8 .|. fromIntegral b

packBytes2L :: [Word8] -> Maybe Word16
packBytes2L (a : b : _) = Just $ packBytes2 a b
packBytes2L _ = Nothing

unpackBytes2 :: Word16 -> (Word8, Word8)
unpackBytes2 n = (fromIntegral (shiftR (n .&. 0xFF00) 8), fromIntegral (n .&. 0xFF))

unpackBytes2L :: Word16 -> [Word8]
unpackBytes2L n = [fromIntegral (shiftR (n .&. 0xFF00) 8), fromIntegral (n .&. 0xFF)]

unpackNibbles2 :: Word8 -> (Word8, Word8)
unpackNibbles2 n = (shiftR (n .&. 0xF0) 4, n .&. 0x0F)

unpackNibbles4 :: Word16 -> (Word8, Word8, Word8, Word8)
unpackNibbles4 n =
  ( fromIntegral (shiftR (n .&. 0xF000) 12),
    fromIntegral (shiftR (n .&. 0xF00) 8),
    fromIntegral (shiftR (n .&. 0xF0) 4),
    fromIntegral (n .&. 0x0F)
  )