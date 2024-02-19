module Machine where

import Control.Monad.State.Strict
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

type MachineST = StateT Machine IO

getRegV_ :: Word8 -> Machine -> Word8
getRegV_ idx m = regv m ! fromIntegral idx

getRegV :: Word8 -> MachineST Word8
getRegV idx = getRegV_ idx <$> get

updateRegI :: Word16 -> MachineST ()
updateRegI v = modify $ \m -> m {regi = v}

updateRegV :: Word8 -> Word8 -> MachineST ()
updateRegV idx v = modify $ \m -> m {regv = regv m // [(fromIntegral idx, v)]}

updateRegPc :: Word16 -> MachineST ()
updateRegPc v = modify $ \m -> m {regpc = v}

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
