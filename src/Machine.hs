module Machine where

import Data.Sequence (Seq ((:|>)), fromList, (|>))
import Data.Word (Word16, Word8)
import Helper

data Machine = Machine
  { regv :: [Word8],
    regi :: Word8,
    regpc :: Word16,
    mem :: [Word8],
    stack :: Seq Word8
  }
  deriving (Show)

applyToReg :: (Word8 -> Word8 -> Word8) -> Word8 -> (Word8, Word8) -> Machine -> Machine
applyToReg f index (valueA, valueB) machine = machine {regv = newregv}
  where
    value = mergeNibble valueA valueB
    newregv = replaceNth index (f value) $ regv machine

-- | f = \\\src dst -> new dst
applyTo2Regs :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> Machine -> Machine
applyTo2Regs f dstIndex srcIndex machine = machine {regv = newregv}
  where
    src = regv machine !! fromIntegral srcIndex
    newF = f src
    newregv = replaceNth dstIndex newF $ regv machine

applyToPc :: (Word16 -> Word16) -> Machine -> Machine
applyToPc f machine = machine {regpc = f . regpc $ machine}

pushStack :: Word8 -> Machine -> Machine
pushStack v machine = machine {stack = newstack}
  where
    newstack = stack machine |> v

pushStack16 :: Word16 -> Machine -> Machine
pushStack16 v machine = machine {stack = newstack}
  where
    newstack = stack machine <> dataSeq
    tupleToList (x, y) = [x, y]
    dataSeq = (fromList . tupleToList) $ extractBytes16 v

popStack :: Machine -> (Machine, Maybe Word8)
popStack machine = case stack machine of
  (rest :|> v) -> (machine {stack = rest}, Just v)
  _ -> (machine, Nothing)

popStack16 :: Machine -> (Machine, Maybe Word16)
popStack16 machine = case stack machine of
  (rest :|> a :|> b) -> (machine {stack = rest}, Just (mergeByte8 a b))
  _ -> (machine, Nothing)