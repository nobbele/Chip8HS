{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Chip8 where

import Control.Monad.State.Strict
import Data.Bits
import Data.Maybe
import Data.Word (Word16, Word8)
import Debug.Trace (trace)
import Helper
import Machine
import Memory
import System.Random.Stateful
import GHC.Base
import qualified Data.Vector.Generic as GV
import qualified Data.Vector as V

selectMathOperator :: Word8 -> Word8 -> Word8 -> Word8
selectMathOperator sel = case sel of
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
  :: Word8 -> Word8 -> Word8

data OpcodeResult = Continue | Skip | Jump Word16

runOpcode :: (Word8, Word8, Word8, Word8) -> MachineST OpcodeResult
runOpcode (0, 0, 0xE, 0) = do
  clearFrameBuffer
  return Continue
runOpcode (0, 0, 0xE, 0xE) = do
  returnAddress <- popStack2
  return . Jump $ fromJust returnAddress
runOpcode (1, a, b, c) = return . Jump $ packNibbles3 a b c
runOpcode (2, a, b, c) = do
  pushStack2 =<< gets regpc
  return . Jump $ packNibbles3 a b c
runOpcode (3, idx, a, b) = do
  let cmpValue = packNibbles2 a b
  regValue <- getRegV idx
  return $ if regValue == cmpValue
    then Skip
    else Continue
runOpcode (4, idx, a, b) = do
  let cmpValue = packNibbles2 a b
  regValue <- getRegV idx
  return $ if regValue /= cmpValue
    then Skip
    else Continue
runOpcode (5, xIdx, yIdx, 0) = do
  xValue <- getRegV xIdx
  yValue <- getRegV yIdx
  return $ if xValue == yValue
    then Skip
    else Continue
runOpcode (6, idx, a, b) = do
  let value = packNibbles2 a b
  updateRegV idx value
  return Continue
runOpcode (7, idx, a, b) = do
  let value = packNibbles2 a b
  regValue <- getRegV idx
  updateRegV idx $ regValue + value
  return Continue
runOpcode (8, dstIdx, srcIdx, sel) = do
  dstVal <- getRegV dstIdx
  srcVal <- getRegV srcIdx
  let op = selectMathOperator sel

  -- Updates the destination register with the result of the math operation
  let mathResult = dstVal `op` srcVal
  updateRegV dstIdx mathResult

  -- Updates the VF register in case of overflow/underflow
  vfValue <- case sel of
      4 -> return $ if (toInteger dstVal + toInteger srcVal) >= 256 then 1 else 0
      5 -> return $ if srcVal > dstVal then 0 else 1
      6 -> return $ dstVal .&. 0x01
      7 -> return $ if dstVal > srcVal then 0 else 1
      8 -> return $ dstVal .&. 0x80
      _ -> getRegV 0xF
  updateRegV 0xF vfValue

  return Continue
runOpcode (9, xIdx, yIdx, 0) = do
  xValue <- getRegV xIdx
  yValue <- getRegV yIdx
  return $ if xValue /= yValue
    then Skip
    else Continue
runOpcode (0xA, a, b, c) = do
  updateRegI $ packNibbles3 a b c
  return Continue
runOpcode (0xB, a, b, c) = do
  pc <- gets regpc
  return . Jump $ pc + packNibbles3 a b c
runOpcode (0xC, idx, a, b) = do
  value <- liftIO $ getStdRandom genWord8

  let mask = packNibbles2 a b
  let maskedValue = value .&. mask
  updateRegV idx maskedValue

  return Continue
runOpcode (0xD, xIdx, yIdx, h) = do
  x <- getRegV xIdx
  y <- getRegV yIdx
  i <- gets regi
  let drawRow offsetY = do
      memory <- gets mem
      let spriteRow = memory GV.! (fromIntegral i + offsetY)
          drawColumn offsetX = do
            let p = spriteRow `testBit` (7 - offsetX)
                y' = y + fromIntegral offsetY
                x' = x + fromIntegral offsetX
            writeFrameBuffer x' y' p
      and <$> V.generateM 8 drawColumn
  updateRegV 0xF . fromIntegral . fromEnum . and =<< V.generateM (fromIntegral h) drawRow
  return Continue
runOpcode (0xF, xIdx, 0x2, 0x9) = do
  c <- getRegV xIdx
  liftIO . print $ c
  let cAddress = fromIntegral c * 5
  updateRegI cAddress
  return Continue
runOpcode _ = return $ trace "Invalid opcode" Continue

-- TODO EX9E
-- TODO EXA1
-- TODO FX07
-- TODO FX0A
-- TODO FX15
-- TODO FX18
-- TODO FX1E
-- TODO FX33
-- TODO FX55
-- TODO FX65

runOpcodeByte :: Word16 -> MachineST ()
runOpcodeByte op = do
  r <- runOpcode $ unpackNibbles4 op
  currentPc <- gets regpc
  case r of
    Continue -> updateRegPc (currentPc + 2)
    Skip -> updateRegPc (currentPc + 4)
    Jump t -> updateRegPc t

runCycleST :: MachineST Bool
runCycleST = do
  memory <- gets mem
  currentPc <- gets regpc
  case readWord currentPc memory of
    Just opcode -> do
      runOpcodeByte opcode
      return True
    Nothing -> return False

runCycle :: Machine -> IO (Bool, Machine)
runCycle = runStateT runCycleST

runMachineST :: MachineST ()
runMachineST = do
  r <- runCycleST
  when r runMachineST

runMachine :: Machine -> IO Machine
runMachine = execStateT runMachineST