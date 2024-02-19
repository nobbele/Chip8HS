{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Chip8 where

import Data.Bits
import Data.Functor
import Data.Maybe
import Data.Word (Word16, Word8)
import Debug.Trace (trace)
import Helper
import Machine
import Memory
import System.Random.Stateful

runConditional :: Bool -> Machine -> Word16
runConditional cond machine = if cond then currentPc + 4 else currentPc + 2
  where
    currentPc = regpc machine

runConditional1 :: (Word8 -> Bool) -> Word8 -> Machine -> Word16
runConditional1 cond vIndex machine = runConditional (cond currentV) machine
  where
    currentV = regvi vIndex machine

runConditional2 :: (Word8 -> Word8 -> Bool) -> Word8 -> Word8 -> Machine -> Word16
runConditional2 cond vIndex vIndex2 machine = runConditional (cond currentVx currentVy) machine
  where
    currentVx = regvi vIndex machine
    currentVy = regvi vIndex2 machine

extractNibbles3 :: (Word8, Word8, Word8, Word8) -> Word16
extractNibbles3 (_, valueA, valueB, valueC) = mergeNibble3 valueA valueB valueC

performRand :: Word8 -> Word8 -> Machine -> IO Machine
performRand vIndex mask machine = ioValue <&> (\value -> updateRegV (value .&. mask) vIndex machine)
  where
    ioValue = getStdRandom genWord8 :: IO Word8

data OpcodeResult = Continue | Jump Word16 | VmIO (IO Machine)

runOpcode :: (Word8, Word8, Word8, Word8) -> Machine -> (OpcodeResult, Machine)
-- return
runOpcode (0, 0, 0xE, 0xE) machine = (Jump $ fromJust returnAddress, newMachine)
  where
    (returnAddress, newMachine) = popStack16 machine
-- jmp NNN
runOpcode op@(1, _, _, _) machine = (Jump $ extractNibbles3 op, machine)
-- call NNN
runOpcode op@(2, _, _, _) machine = (Jump $ extractNibbles3 op, pushStack16 (regpc machine) machine)
-- if (Vx == NN): skip
runOpcode (3, vIndex, valueA, valueB) machine = (Jump $ runConditional1 (== cmpValue) vIndex machine, machine)
  where
    cmpValue = mergeNibble valueA valueB
-- if (Vx != NN): skip
runOpcode (4, vIndex, valueA, valueB) machine = (Jump $ runConditional1 (/= cmpValue) vIndex machine, machine)
  where
    cmpValue = mergeNibble valueA valueB
-- if (Vx == Vy): skip
runOpcode (5, vIndex, vIndex2, 0) machine = (Jump $ runConditional2 (==) vIndex vIndex2 machine, machine)
-- Vx = NN
runOpcode (6, vIndex, valueA, valueB) machine = (Continue, applyToReg (\_ x -> x) vIndex (valueA, valueB) machine)
-- Vx += NN
runOpcode (7, vIndex, valueA, valueB) machine = (Continue, applyToReg (+) vIndex (valueA, valueB) machine)
runOpcode (8, dstIndex, srcIndex, c) machine = (Continue, updateRegV valueVF 0xF (applyTo2Regs mathOp dstIndex srcIndex machine))
  where
    mathOp = case c of
      0 -> (\_ src -> src)
      1 -> (.|.)
      2 -> (.&.)
      3 -> (.^.)
      4 -> (+)
      5 -> (-)
      6 -> (\dst _ -> dst `shiftR` 1)
      7 -> flip (-)
      0xE -> (\dst _ -> dst `shiftL` 1)
      _ -> error "Invalid math operation"
    valueVF = case c of
      4 -> if (toInteger dstVal + toInteger srcVal) >= 256 then 1 else 0
      5 -> if srcVal > dstVal then 0 else 1
      6 -> dstVal .&. 0x01
      7 -> if dstVal > srcVal then 0 else 1
      8 -> dstVal .&. 0x80
      _ -> regvi 0xF machine
      where
        dstVal = regvi dstIndex machine
        srcVal = regvi srcIndex machine
-- if (Vx != Vy): skip
runOpcode (9, vIndex, vIndex2, 0) machine = (Jump $ runConditional2 (/=) vIndex vIndex2 machine, machine)
-- I = NNN
runOpcode op@(0xA, _, _, _) machine = (Continue, updateI (extractNibbles3 op) machine)
runOpcode (0xC, vIndex, valueA, valueB) machine = (VmIO $ performRand vIndex value machine, machine)
  where
    value = mergeNibble valueA valueB
runOpcode _ machine = trace "Invalid opcode" (Continue, machine)

runOpcodeByte :: Word16 -> Machine -> (OpcodeResult, Machine)
runOpcodeByte = runOpcode . extractNibbles16

finishCycle :: OpcodeResult -> Machine -> IO Machine
finishCycle result machine = case result of
  Continue -> return $ applyToPc (+ 2) machine
  Jump dst -> return $ machine {regpc = dst}
  VmIO ioMachine -> applyToPc (+ 2) <$> ioMachine

runMachine :: Machine -> IO Machine
runMachine machine = case maybeOpcode of
  Just opcode -> runMachine =<< (finishCycle_ . runOpcodeByte opcode) machine
    where
      finishCycle_ = uncurry finishCycle
  Nothing -> return machine
  where
    maybeOpcode = readWord (regpc machine) (mem machine)
