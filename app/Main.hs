module Main where

import Chip8
import Data.Vector.Generic
import Data.Word (Word8)
import Machine
import Prelude hiding (replicate)

fibonacciCode :: [Word8]
fibonacciCode =
  [ --- Parameters ---
    -- V2 = K
    0x62,
    9,
    -- V0 = 0
    0x60,
    0,
    -- V1 = 1
    0x61,
    1,
    ----------------------
    -- V2 -= 2
    0x72,
    0xFE, -- (-2)
    -- loop: (0x008)
    -- V2 -= 1
    0x72,
    0xFF, -- (-1)
    -- V3 = V0
    0x83,
    0x00,
    -- V3 += V1
    0x83,
    0x14,
    -- V0 = V1
    0x80,
    0x10,
    -- V1 = V3
    0x81,
    0x30,
    -- if (V2 == 0) skip
    0x32,
    0x00,
    -- jmp loop (0x008)
    0x10,
    0x08,
    -- V0 = V1
    0x80,
    0x10
  ]

main :: IO ()
main = print =<< machine
  where
    defaultMachine =
      Machine
        { regv = replicate 16 (0 :: Word8),
          regi = 0,
          regpc = 0x0,
          mem = fromList fibonacciCode,
          stack = []
        }
    machine = runMachine defaultMachine