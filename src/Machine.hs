module Machine where

import Data.Vector.Unboxed (Vector, (!), (//))
import Data.Word (Word16, Word8)
import Helper

data Machine = Machine
  { regv :: Vector Word8,
    regi :: Word16,
    regpc :: Word16,
    mem :: Vector Word8,
    stack :: [Word8]
  }
  deriving (Show)

updateI :: Word16 -> Machine -> Machine
updateI v machine = machine {regi = v}

updateRegV :: Word8 -> Word8 -> Machine -> Machine
updateRegV v vIndex machine = machine {regv = newregv}
  where
    -- newregv = replaceNth vIndex (const v) $ regv machine
    newregv = regv machine // [(fromIntegral vIndex, v)]

regvi :: Word8 -> Machine -> Word8
regvi index machine = regv machine ! fromIntegral index

-- | f = \\\val dst -> new dst
applyToReg :: (Word8 -> Word8 -> Word8) -> Word8 -> (Word8, Word8) -> Machine -> Machine
applyToReg f index (valueA, valueB) machine = machine {regv = newregv}
  where
    value = mergeNibble valueA valueB
    dstValue = regv machine ! fromIntegral index
    newregv = regv machine // [(fromIntegral index, f dstValue value)]

-- | f = \\\dst src -> new dst
applyTo2Regs :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> Machine -> Machine
applyTo2Regs f dstIndex srcIndex machine = machine {regv = newregv}
  where
    src = regvi srcIndex machine
    dst = regvi dstIndex machine
    newregv = regv machine // [(fromIntegral dstIndex, f dst src)]

applyToPc :: (Word16 -> Word16) -> Machine -> Machine
applyToPc f machine = machine {regpc = f . regpc $ machine}

pushStack :: Word8 -> Machine -> Machine
pushStack v machine = machine {stack = newstack}
  where
    newstack = v : stack machine

pushStack16 :: Word16 -> Machine -> Machine
pushStack16 v machine = machine {stack = newstack}
  where
    newstack = a : b : stack machine
    (a, b) = extractBytes16 v

popStack :: Machine -> (Maybe Word8, Machine)
popStack machine = case stack machine of
  (top : rest) -> (Just top, machine {stack = rest})
  _ -> (Nothing, machine)

popStack16 :: Machine -> (Maybe Word16, Machine)
popStack16 machine = case stack machine of
  (a : b : rest) -> (Just $ mergeByte8 a b, machine {stack = rest})
  _ -> (Nothing, machine)
