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
import Numeric (showHex)
import GHC.Num (integerDiv)

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

skipWhen :: Bool -> MachineST OpcodeResult
skipWhen b = return $ if b then Skip else Continue

skipUnless :: Bool -> MachineST OpcodeResult
skipUnless = skipWhen <$> not

getRegV2 :: Word8 -> Word8 -> MachineST (Word8, Word8)
getRegV2 a b = do
  x <- getRegV a
  y <- getRegV b
  return (x, y)

bcd :: Word8 -> (Word8, Word8, Word8)
bcd n = (hundreds,tens, ones)
  where ones = n `rem` 10
        tens = fromIntegral (fromIntegral n `integerDiv` 10) `rem` 10
        hundreds = fromIntegral (fromIntegral n `integerDiv` 100) `rem` 10

runOpcode :: (Word8, Word8, Word8, Word8) -> MachineST OpcodeResult
runOpcode (0, 0, 0xE, 0) = do
  clearFrameBuffer
  return Continue
runOpcode (0, 0, 0xE, 0xE) = do
  returnAddress <- popStack2
  return . Jump . (+2) $ fromJust returnAddress
runOpcode (1, a, b, c) = return . Jump $ packNibbles3 a b c
runOpcode (2, a, b, c) = do
  pushStack2 =<< gets regpc
  return . Jump $ packNibbles3 a b c
runOpcode (3, idx, a, b) = do
  let cmpValue = packNibbles2 a b
  regValue <- getRegV idx
  skipWhen $ regValue == cmpValue
runOpcode (4, idx, a, b) = do
  let cmpValue = packNibbles2 a b
  regValue <- getRegV idx
  skipWhen $ regValue /= cmpValue
runOpcode (5, xIdx, yIdx, 0) = do
  (x, y) <- getRegV2 xIdx yIdx
  skipWhen $ x == y
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
  (x, y) <- getRegV2 xIdx yIdx
  skipWhen $ x /= y
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
  (x, y) <- getRegV2 xIdx yIdx
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
  let cAddress = fromIntegral c * 5
  updateRegI cAddress
  return Continue
runOpcode (0xE, xIdx, 0x9, 0xE) = skipWhen =<< getKeyDown =<< getRegV xIdx
runOpcode (0xE, xIdx, 0xA, 0x1) = skipUnless =<< getKeyDown =<< getRegV xIdx
runOpcode (0xF, xIdx, 0x0, 0x7) = do
  v <- gets delayTimer
  updateRegV v xIdx
  return Continue
runOpcode (0xF, xIdx, 0x1, 0x5) = do
  updateDelayTimer =<< getRegV xIdx
  return Continue
runOpcode (0xF, xIdx, 0x1, 0x8) = do
  updateSoundTimer =<< getRegV xIdx
  return Continue
runOpcode (0xF, xIdx, 0x1, 0xE) = do
  i <- gets regi
  x <- fromIntegral <$> getRegV xIdx
  updateRegI $ i + x
  return Continue
runOpcode (0xF, xIdx, 0x3, 0x3) = do
  (hundreds, tens, ones) <- bcd <$> getRegV xIdx
  i <- fromIntegral <$> gets regi
  modify $ \m -> m {mem = mem m GV.// [(i, hundreds), (i + 1, tens), (i + 2, ones)]}
  return Continue
runOpcode (0xF, xIdx, 0x5, 0x5) = do
  v <- GV.take (fromIntegral xIdx + 1) <$> gets regv
  i <- fromIntegral <$> gets regi
  let updateList = GV.imap (\idx val -> (i + idx, val)) v
  modify $ \m -> m {mem = mem m `GV.update` updateList}
  return Continue
runOpcode (0xF, xIdx, 0x6, 0x5) = do
  m <- gets mem
  v <- GV.take (fromIntegral xIdx + 1) <$> gets regv
  i <- fromIntegral <$> gets regi
  GV.imapM_ (\idx _ -> updateRegV (fromIntegral idx) $ m GV.! (i + idx)) v
  return Continue
runOpcode (a, b, c, d) = return $ trace ("Invalid opcode " ++ showHex (packNibbles4 a b c d) "") Continue

-- TODO FX0A

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
      -- lift . print $ showHex opcode ""
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