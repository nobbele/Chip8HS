{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Machine where

import Control.Monad.State.Strict
import Data.Bits ((.^.))
import qualified Data.Vector as V
import Data.Vector.Generic ((!), (!?), (++), (//))
import qualified Data.Vector.Unboxed as UV
import Data.Word (Word16, Word8)
import Helper
import Prelude hiding (replicate, (++))

type FrameBufferRow = UV.Vector Bool

type FrameBuffer = V.Vector FrameBufferRow

data Machine = Machine
  { regv :: UV.Vector Word8,
    regi :: Word16,
    regpc :: Word16,
    mem :: UV.Vector Word8,
    stack :: [Word8],
    frameBuffer :: FrameBuffer
  }

defaultFrameBuffer :: FrameBuffer
defaultFrameBuffer = V.replicate 32 (UV.replicate 64 False)

defaultFontBuffer :: [Word8]
defaultFontBuffer =
  [ 0xF0,
    0x90,
    0x90,
    0x90,
    0xF0,
    0x20,
    0x60,
    0x20,
    0x20,
    0x70,
    0xF0,
    0x10,
    0xF0,
    0x80,
    0xF0,
    0xF0,
    0x10,
    0xF0,
    0x10,
    0xF0,
    0x90,
    0x90,
    0xF0,
    0x10,
    0x10,
    0xF0,
    0x80,
    0xF0,
    0x10,
    0xF0,
    0xF0,
    0x80,
    0xF0,
    0x90,
    0xF0,
    0xF0,
    0x10,
    0x20,
    0x40,
    0x40,
    0xF0,
    0x90,
    0xF0,
    0x90,
    0xF0,
    0xF0,
    0x90,
    0xF0,
    0x10,
    0xF0,
    0xF0,
    0x90,
    0xF0,
    0x90,
    0x90,
    0xE0,
    0x90,
    0xE0,
    0x90,
    0xE0,
    0xF0,
    0x80,
    0x80,
    0x80,
    0xF0,
    0xE0,
    0x90,
    0x90,
    0x90,
    0xE0,
    0xF0,
    0x80,
    0xF0,
    0x80,
    0xF0,
    0xF0,
    0x80,
    0xF0,
    0x80,
    0x80
  ]

defaultMachine :: Machine
defaultMachine =
  Machine
    { regv = UV.replicate 16 (0 :: Word8),
      regi = 0,
      regpc = 0x200,
      mem = UV.fromList defaultFontBuffer ++ UV.replicate (0x200 - length defaultFontBuffer) (0 :: Word8),
      stack = [],
      frameBuffer = defaultFrameBuffer
    }

appendToMemory :: [Word8] -> Machine -> Machine
appendToMemory l m = m {mem = mem m ++ UV.fromList l}

appendProgram :: [Word16] -> Machine -> Machine
appendProgram l = appendToMemory $ concatMap unpackBytes2L l

type MachineST = StateT Machine IO

getRegV_ :: Word8 -> Machine -> Word8
getRegV_ idx m = regv m ! fromIntegral idx

getRegV :: Word8 -> MachineST Word8
getRegV idx = getRegV_ idx <$> get

getFrameBuffer :: MachineST FrameBuffer
getFrameBuffer = frameBuffer <$> get

updateRegI :: Word16 -> MachineST ()
updateRegI v = modify $ \m -> m {regi = v}

updateRegV :: Word8 -> Word8 -> MachineST ()
updateRegV idx v = modify $ \m -> m {regv = regv m // [(fromIntegral idx, v)]}

updateRegPc :: Word16 -> MachineST ()
updateRegPc v = modify $ \m -> m {regpc = v}

writeFrameBuffer :: Word8 -> Word8 -> Bool -> MachineST Bool
writeFrameBuffer x y v = do
  fb <- gets frameBuffer
  let r = do
        row <- fb !? fromIntegral y
        p <- row !? fromIntegral x
        return (row, p)
  case r of
    Just (row, p) -> do
      let flipped = p && v
          row' = row // [(fromIntegral x, p .^. v)]
          fb' = fb // [(fromIntegral y, row')]
      modify $ \m -> m {frameBuffer = fb'}
      return flipped
    Nothing -> return False

clearFrameBuffer :: MachineST ()
clearFrameBuffer = modify $ \m -> m {frameBuffer = defaultFrameBuffer}

mapFrameBuffer :: (Monad m) => (Int -> Int -> Bool -> m ()) -> Machine -> m ()
mapFrameBuffer f = V.imapM_ (UV.imapM_ . flip f) . frameBuffer

applyToReg :: (Word8 -> Word8) -> Word8 -> MachineST ()
applyToReg f idx = do
  old <- getRegV idx
  let new = f old
  updateRegV new idx

applyToRegs2 :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> MachineST ()
applyToRegs2 f dstIdx srcIdx = do
  old <- getRegV dstIdx
  src <- getRegV srcIdx
  let new = f old src
  updateRegV new dstIdx

updatePc :: Word16 -> MachineST ()
updatePc v = modify $ \m -> m {regpc = v}

pushStack :: Word8 -> MachineST ()
pushStack v = modify $ \m -> m {stack = v : stack m}

pushStack2 :: Word16 -> MachineST ()
pushStack2 v = do
  let (a, b) = unpackBytes2 v
  modify $ \m -> m {stack = a : b : stack m}

popStack :: MachineST (Maybe Word8)
popStack = do
  m <- get
  case stack m of
    (v : rest) -> do
      put $ m {stack = rest}
      return $ Just v
    _ -> return Nothing

popStack2 :: MachineST (Maybe Word16)
popStack2 = do
  m <- get
  case stack m of
    (a : b : rest) -> do
      put $ m {stack = rest}
      return . Just $ packBytes2 a b
    _ -> return Nothing
