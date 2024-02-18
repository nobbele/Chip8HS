{-# HLINT ignore "Use head" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Chip8
import Data.Sequence (empty)
import Data.Word (Word8)
import Machine
import Test.HUnit

defaultMachine :: Machine
defaultMachine =
  Machine
    { regv = replicate 16 (0 :: Word8),
      regi = 0,
      regpc = 0x0,
      mem = [],
      stack = empty
    }

testAssign :: Test
testAssign =
  TestCase
    ( do
        let machine = runMachine defaultMachine {mem = [0x60, 16]}
        assertEqual "V0 = 16" 16 (regv machine !! 0)
    )

testMath :: Test
testMath =
  TestCase
    ( do
        let machine = runMachine defaultMachine {mem = [0x60, 16, 0x70, 2]}
        assertEqual "V0 = 16; V0 += 2" 18 (regv machine !! 0)

        let machine = runMachine defaultMachine {mem = [0x60, 16, 0x81, 0x00]}
        assertEqual "V0 = 16; V1 = V0 (0)" (regv machine !! 0) (regv machine !! 1)
        assertEqual "V0 = 16; V1 = V0 (1)" 16 (regv machine !! 1)

        let machine = runMachine defaultMachine {mem = [0x60, 0x10, 0x61, 0x5, 0x80, 0x11]}
        assertEqual "V0 = 16; V1 = 5; V0 |= V1 (0)" 0x15 (regv machine !! 0)
        assertEqual "V0 = 16; V1 = 5; V0 |= V1 (1)" 0x05 (regv machine !! 1)

        let machine = runMachine defaultMachine {mem = [0x60, 0x0E, 0x61, 0x5, 0x80, 0x12]}
        assertEqual "V0 = 14; V1 = 5; V0 &= V1 (0)" 0x04 (regv machine !! 0)
        assertEqual "V0 = 14; V1 = 5; V0 &= V1 (1)" 0x05 (regv machine !! 1)

        let machine = runMachine defaultMachine {mem = [0x60, 0x0E, 0x61, 0x5, 0x80, 0x13]}
        assertEqual "V0 = 14; V1 = 5; V0 ^= V1 (0)" 0x0B (regv machine !! 0)
        assertEqual "V0 = 14; V1 = 5; V0 ^= V1 (1)" 0x05 (regv machine !! 1)

        let machine = runMachine defaultMachine {mem = [0x60, 14, 0x61, 5, 0x80, 0x14]}
        assertEqual "V0 = 14; V1 = 5; V0 += V1 (0)" 19 (regv machine !! 0)
        assertEqual "V0 = 14; V1 = 5; V0 += V1 (1)" 5 (regv machine !! 1)

        let machine = runMachine defaultMachine {mem = [0x60, 14, 0x61, 5, 0x80, 0x15]}
        assertEqual "V0 = 14; V1 = 5; V0 -= V1 (0)" 9 (regv machine !! 0)
        assertEqual "V0 = 14; V1 = 5; V0 -= V1 (1)" 5 (regv machine !! 1)

        let machine = runMachine defaultMachine {mem = [0x60, 0x5, 0x80, 0x16]}
        assertEqual "V0 = 5; V0 >>= 1 (0)" 0x02 (regv machine !! 0)

        let machine = runMachine defaultMachine {mem = [0x60, 5, 0x61, 14, 0x80, 0x17]}
        assertEqual "V0 = 5; V1 = 14; V0 = V1 - V0 (0)" 9 (regv machine !! 0)
        assertEqual "V0 = 5; V1 = 14; V0 = V1 - V0 (1)" 14 (regv machine !! 1)

        let machine = runMachine defaultMachine {mem = [0x60, 0x5, 0x80, 0x1E]}
        assertEqual "V0 = 5; V0 <<= 1" 0x0A (regv machine !! 0)
    )

testControlFlow :: Test
testControlFlow =
  TestCase
    ( do
        let machine = runMachine defaultMachine {mem = [0x60, 16, 0x10, 0x06, 0x60, 5, 0x61, 10]}
        assertEqual "V0 = 16; goto after; V0 = 5; after: V1 = 10 (0)" 16 (regv machine !! 0)
        assertEqual "V0 = 16; goto after; V0 = 5; after: V1 = 10 (0)" 10 (regv machine !! 1)
    )

main :: IO Counts
main = do
  runTestTT $
    TestList
      [ TestLabel "testAssign" testAssign,
        TestLabel "testMath" testMath,
        TestLabel "testControlFlow" testControlFlow
      ]