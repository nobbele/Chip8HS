{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

import Chip8
import Data.Vector.Generic (replicate, (!))
import Data.Vector.Unboxed (fromList)
import Data.Word (Word8)
import Machine
import Test.HUnit
import Prelude hiding (replicate)

defaultMachine :: Machine
defaultMachine =
  Machine
    { regv = replicate 16 (0 :: Word8),
      regi = 0,
      regpc = 0x0,
      mem = fromList [],
      stack = []
    }

testAssign :: Test
testAssign =
  TestCase
    ( do
        machine <- runMachine defaultMachine {mem = fromList [0x60, 16]}
        assertEqual "V0 = 16" 16 (regv machine ! 0)

        machine <- runMachine defaultMachine {mem = fromList [0xA1, 0x16]}
        assertEqual "I = 0x116" 0x116 (regi machine)
    )

testMath :: Test
testMath =
  TestCase
    ( do
        machine <- runMachine defaultMachine {mem = fromList [0x60, 16, 0x70, 2]}
        assertEqual "V0 = 16; V0 += 2" 18 (regv machine ! 0)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 16, 0x81, 0x00]}
        assertEqual "V0 = 16; V1 = V0 (0)" (regv machine ! 0) (regv machine ! 1)
        assertEqual "V0 = 16; V1 = V0 (1)" 16 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 0x10, 0x61, 0x5, 0x80, 0x11]}
        assertEqual "V0 = 16; V1 = 5; V0 |= V1 (0)" 0x15 (regv machine ! 0)
        assertEqual "V0 = 16; V1 = 5; V0 |= V1 (1)" 0x05 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 0x0E, 0x61, 0x5, 0x80, 0x12]}
        assertEqual "V0 = 14; V1 = 5; V0 &= V1 (0)" 0x04 (regv machine ! 0)
        assertEqual "V0 = 14; V1 = 5; V0 &= V1 (1)" 0x05 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 0x0E, 0x61, 0x5, 0x80, 0x13]}
        assertEqual "V0 = 14; V1 = 5; V0 ^= V1 (0)" 0x0B (regv machine ! 0)
        assertEqual "V0 = 14; V1 = 5; V0 ^= V1 (1)" 0x05 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 14, 0x61, 5, 0x80, 0x14]}
        assertEqual "V0 = 14; V1 = 5; V0 += V1 (0)" 19 (regv machine ! 0)
        assertEqual "V0 = 14; V1 = 5; V0 += V1 (1)" 5 (regv machine ! 1)
        assertEqual "V0 = 14; V1 = 5; V0 += V1 (2)" 0 (regvi 0xF machine)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 128, 0x61, 128, 0x80, 0x14]}
        assertEqual "V0 = 128; V1 = 128; V0 += V1 (0)" 0 (regvi 0 machine)
        assertEqual "V0 = 128; V1 = 128; V0 += V1 (1)" 1 (regvi 0xF machine)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 14, 0x61, 5, 0x80, 0x15]}
        assertEqual "V0 = 14; V1 = 5; V0 -= V1 (0)" 9 (regvi 0 machine)
        assertEqual "V0 = 14; V1 = 5; V0 -= V1 (1)" 5 (regvi 1 machine)
        assertEqual "V0 = 14; V1 = 5; V0 -= V1 (2)" 1 (regvi 0xF machine)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 14, 0x61, 16, 0x80, 0x15]}
        assertEqual "V0 = 14; V1 = 16; V0 -= V1 (0)" 254 (regvi 0 machine)
        assertEqual "V0 = 14; V1 = 16; V0 -= V1 (1)" 16 (regvi 1 machine)
        assertEqual "V0 = 14; V1 = 16; V0 -= V1 (2)" 0 (regvi 0xF machine)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 0x5, 0x80, 0x16]}
        assertEqual "V0 = 5; V0 >>= 1 (0)" 0x02 (regv machine ! 0)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 5, 0x61, 14, 0x80, 0x17]}
        assertEqual "V0 = 5; V1 = 14; V0 = V1 - V0 (0)" 9 (regv machine ! 0)
        assertEqual "V0 = 5; V1 = 14; V0 = V1 - V0 (1)" 14 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 0x5, 0x80, 0x1E]}
        assertEqual "V0 = 5; V0 <<= 1" 0x0A (regv machine ! 0)
    )

testControlFlow :: Test
testControlFlow =
  TestCase
    ( do
        machine <- runMachine defaultMachine {mem = fromList [0x60, 16, 0x10, 0x06, 0x60, 5, 0x61, 10]}
        assertEqual "V0 = 16; goto after; V0 = 5; after: V1 = 10 (0)" 16 (regv machine ! 0)
        assertEqual "V0 = 16; goto after; V0 = 5; after: V1 = 10 (0)" 10 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 16, 0x30, 5, 0x10, 0x08, 0x60, 5, 0x61, 10]}
        assertEqual "V0 = 16; if (V0 == 5) skip; goto after; V0 = 5; after: V1 = 10 (0)" 16 (regv machine ! 0)
        assertEqual "V0 = 16; if (V0 == 5) skip; goto after; V0 = 5; after: V1 = 10 (0)" 10 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 16, 0x30, 16, 0x10, 0x08, 0x60, 5, 0x61, 10]}
        assertEqual "V0 = 16; if (V0 == 16) skip; goto after; V0 = 5; after: V1 = 10 (0)" 5 (regv machine ! 0)
        assertEqual "V0 = 16; if (V0 == 16) skip; goto after; V0 = 5; after: V1 = 10 (0)" 10 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 16, 0x40, 5, 0x10, 0x08, 0x60, 5, 0x61, 10]}
        assertEqual "V0 = 16; if (V0 != 5) skip; goto after; V0 = 5; after: V1 = 10 (0)" 5 (regv machine ! 0)
        assertEqual "V0 = 16; if (V0 != 5) skip; goto after; V0 = 5; after: V1 = 10 (0)" 10 (regv machine ! 1)

        machine <- runMachine defaultMachine {mem = fromList [0x60, 16, 0x40, 16, 0x10, 0x08, 0x60, 5, 0x61, 10]}
        assertEqual "V0 = 16; if (V0 != 16) skip; goto after; V0 = 5; after: V1 = 10 (0)" 16 (regv machine ! 0)
        assertEqual "V0 = 16; if (V0 != 16) skip; goto after; V0 = 5; after: V1 = 10 (0)" 10 (regv machine ! 1)

        -- TODO Test if-instructions
    )

main :: IO Counts
main = do
  runTestTT $
    TestList
      [ TestLabel "testAssign" testAssign,
        TestLabel "testMath" testMath,
        TestLabel "testControlFlow" testControlFlow
      ]