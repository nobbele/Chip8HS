{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Chip8 where

import Data.Bits
import Data.Maybe
import Data.Word (Word16, Word8)
import Debug.Trace (trace)
import Helper
import Machine
import Memory

data OpcodeResult = Continue | Jump Word16

runOpcode :: (Word8, Word8, Word8, Word8) -> Machine -> (Machine, OpcodeResult)
-- return
runOpcode (0, 0, 0xE, 0xE) machine = (newMachine, Jump $ fromJust returnAddress)
  where
    (newMachine, returnAddress) = popStack16 machine
-- jmp NNN
runOpcode (1, valueA, valueB, valueC) machine = (machine, Jump $ mergeNibble3 valueA valueB valueC)
-- call NNN
runOpcode (2, valueA, valueB, valueC) machine = (pushStack16 (regpc machine) machine, Jump $ mergeNibble3 valueA valueB valueC)
-- Vx = NN
runOpcode (6, vIndex, valueA, valueB) machine = (applyToReg const vIndex (valueA, valueB) machine, Continue)
-- Vx += NN
runOpcode (7, vIndex, valueA, valueB) machine = (applyToReg (+) vIndex (valueA, valueB) machine, Continue)
-- Vx = Vy
runOpcode (8, dstIndex, srcIndex, 0) machine = (applyTo2Regs const dstIndex srcIndex machine, Continue)
-- Vx = Vy | Vx
runOpcode (8, dstIndex, srcIndex, 1) machine = (applyTo2Regs (.|.) dstIndex srcIndex machine, Continue)
-- Vx = Vy & Vx
runOpcode (8, dstIndex, srcIndex, 2) machine = (applyTo2Regs (.&.) dstIndex srcIndex machine, Continue)
-- Vx = Vy ^ Vx
runOpcode (8, dstIndex, srcIndex, 3) machine = (applyTo2Regs (.^.) dstIndex srcIndex machine, Continue)
-- Vx = Vy + Vx
runOpcode (8, dstIndex, srcIndex, 4) machine = (applyTo2Regs (+) dstIndex srcIndex machine, Continue)
-- Vx = Vx - Vy
runOpcode (8, dstIndex, srcIndex, 5) machine = (applyTo2Regs (flip (-)) dstIndex srcIndex machine, Continue)
-- Vx >>= 1
runOpcode (8, dstIndex, srcIndex, 6) machine = (applyTo2Regs (\_ dst -> dst `shiftR` 1) dstIndex srcIndex machine, Continue)
-- Vx = Vy - Vx
runOpcode (8, dstIndex, srcIndex, 7) machine = (applyTo2Regs (-) dstIndex srcIndex machine, Continue)
-- Vx <<= 1
runOpcode (8, dstIndex, srcIndex, 0xE) machine = (applyTo2Regs (\_ dst -> dst `shiftL` 1) dstIndex srcIndex machine, Continue)
runOpcode _ machine = trace "Invalid opcode" (machine, Continue)

runOpcodeByte :: Word16 -> Machine -> (Machine, OpcodeResult)
runOpcodeByte = runOpcode . extractNibbles16

prepareMachine :: (Machine, OpcodeResult) -> Machine
prepareMachine (machine, result) = case result of
  Continue -> applyToPc (+ 2) machine
  Jump dst -> machine {regpc = dst}

runMachine :: Machine -> Machine
runMachine machine = case maybeOpcode of
  Just opcode -> runMachine . prepareMachine . runOpcodeByte opcode $ machine
  Nothing -> machine
  where
    maybeOpcode = readWord (regpc machine) (mem machine)
