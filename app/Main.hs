module Main where

import Chip8
import Data.Sequence (empty)
import Data.Word (Word8)
import Machine

main :: IO ()
main = print machine
  where
    defaultMachine =
      Machine
        { regv = replicate 16 (0 :: Word8),
          regi = 0,
          regpc = 0x0,
          mem = [0x60, 0x10, 0x6B, 0xCD, 0x7B, 0x01, 0x84, 0xB0, 0x80, 0xB4],
          stack = empty
        }
    machine = runMachine defaultMachine