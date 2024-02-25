module Main where

import Chip8
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as B
import Data.String
import GHC.Num (integerDiv)
import GHC.Word
import Machine
import SDL (Point (P), V2 (V2), V4 (V4), ($=))
import qualified SDL
import Prelude hiding (replicate)

data AppState = AppState
  { window_ :: SDL.Window,
    renderer_ :: SDL.Renderer,
    machine_ :: Machine
  }

type AppST = StateT AppState IO

runMachineCycle :: AppST ()
runMachineCycle = do
  (_, machine') <- liftIO . runCycle =<< gets machine_
  modify' $ \s -> s {machine_ = machine'}

drawPixel :: Int -> Int -> AppST ()
drawPixel x y = do
  renderer <- gets renderer_
  window <- gets window_

  SDL.V2 cwindowSizeX cwindowSizeY <- SDL.get (SDL.windowSize window)

  let windowSizeX = fromIntegral cwindowSizeX
      windowSizeY = fromIntegral cwindowSizeY

  let pixelSizeX = windowSizeX / 64
      pixelSizeY = windowSizeY / 32

  SDL.rendererDrawColor renderer $= V4 255 255 255 255
  SDL.fillRectF renderer $ SDL.Rectangle (P $ V2 (fromIntegral x * pixelSizeX) (fromIntegral y * pixelSizeY)) (V2 pixelSizeX pixelSizeY)

draw :: AppST ()
draw = do
  machine <- gets machine_

  mapFrameBuffer (\x y p -> when p $ drawPixel x y) machine

keyOfKbEvent :: SDL.KeyboardEventData -> SDL.Keycode
keyOfKbEvent = SDL.keysymKeycode . SDL.keyboardEventKeysym

handleKeyUpdateEvent :: SDL.Event -> AppST ()
handleKeyUpdateEvent e = case SDL.eventPayload e of
  SDL.KeyboardEvent keyboardEvent -> case getNum . keyOfKbEvent $ keyboardEvent of
    Just key -> modify $ \st -> st {machine_ = updateKey_ key (SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed) $ machine_ st}
    Nothing -> return ()
  _ -> return ()
  where
    getNum :: SDL.Keycode -> Maybe Word8
    getNum SDL.Keycode1 = Just 1
    getNum SDL.Keycode2 = Just 2
    getNum SDL.Keycode3 = Just 3
    getNum SDL.Keycode4 = Just 0xC
    getNum SDL.KeycodeQ = Just 4
    getNum SDL.KeycodeW = Just 5
    getNum SDL.KeycodeE = Just 6
    getNum SDL.KeycodeR = Just 0xD
    getNum SDL.KeycodeA = Just 7
    getNum SDL.KeycodeS = Just 8
    getNum SDL.KeycodeD = Just 9
    getNum SDL.KeycodeF = Just 0xE
    getNum SDL.KeycodeZ = Just 0xA
    getNum SDL.KeycodeX = Just 0
    getNum SDL.KeycodeC = Just 0xB
    getNum SDL.KeycodeV = Just 0xF
    getNum _ = Nothing

appLoop :: AppST ()
appLoop = do
  events <- SDL.pollEvents
  let escPressed = flip any events $ \e -> case SDL.eventPayload e of
        SDL.KeyboardEvent keyboardEvent ->
          SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
            && keyOfKbEvent keyboardEvent == SDL.KeycodeEscape
        SDL.WindowClosedEvent _ -> True
        _ -> False

  forM_ events handleKeyUpdateEvent

  -- TODO 60hz timing
  modify $ \st -> st {machine_ = tickTimers_ $ machine_ st}

  -- TODO 500hz timing
  runMachineCycle

  renderer <- gets renderer_
  SDL.rendererDrawColor renderer $= V4 0 0 255 255
  SDL.clear renderer
  draw
  SDL.present renderer

  SDL.delay $ fromIntegral (1000 `integerDiv` 144)
  unless escPressed appLoop

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow (fromString "My SDL Application") SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  programCode <- B.unpack <$> B.readFile "test_opcode.ch8"

  evalStateT appLoop $
    AppState window renderer $
      fullMachineWithProgram programCode

  SDL.destroyWindow window
  return ()
