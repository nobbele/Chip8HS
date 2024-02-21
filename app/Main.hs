{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Chip8
import Control.Monad
import Control.Monad.State.Strict
import Data.String
import GHC.Num (integerDiv)
import GHC.Word
import Machine
import SDL (Point (P), V2 (V2), V4 (V4), ($=))
import qualified SDL
import Prelude hiding (replicate)

fibonacciCode :: [Word16]
fibonacciCode =
  [ --- Parameters ---
    -- V2 = K
    0x6210,
    -- V0 = 0
    0x6000,
    -- V1 = 1
    0x6101,
    ----------------------
    -- V2 -= 2
    0x72FE, -- (-2)
    -- loop: (0x008)
    -- V2 -= 1
    0x72FF, -- (-1)
    -- V3 = V0
    0x8300,
    -- V3 += V1
    0x8314,
    -- V0 = V1
    0x8010,
    -- V1 = V3
    0x8130,
    -- draw(V0, V1, 0)
    0xD210,
    -- if (V2 == 0) skip
    0x3200,
    -- jmp loop (0x208)
    0x1208,
    -- V0 = V1
    0x8010
  ]

abcCode :: [Word16]
abcCode =
  [ -- V0 = 0x0A
    0x600A,
    -- V1 = 0x00
    0x6100,
    -- V2 = 0x00
    0x6200,
    -- I = sprite_addr[V0]
    0xF029,
    -- draw(V1, V2, 5),
    0xD125,
    -- V0 += 1
    0x7001,
    -- I = sprite_addr[V0]
    0xF029,
    -- V1 += 5
    0x7105,
    -- draw(V1, V2, 5),
    0xD125,
    -- V0 += 1
    0x7001,
    -- I = sprite_addr[V0]
    0xF029,
    -- V1 += 5
    0x7105,
    -- draw(V1, V2, 5),
    0xD125
  ]

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

drawPixel :: SDL.Renderer -> SDL.Window -> Int -> Int -> IO ()
drawPixel renderer window x y = do
  SDL.V2 cwindowSizeX cwindowSizeY <- SDL.get (SDL.windowSize window)

  let windowSizeX = fromIntegral cwindowSizeX
      windowSizeY = fromIntegral cwindowSizeY

  let pixelSizeX = windowSizeX / 64
      pixelSizeY = windowSizeY / 32

  SDL.rendererDrawColor renderer $= V4 255 255 255 255
  SDL.fillRectF renderer $ SDL.Rectangle (P $ V2 (fromIntegral x * pixelSizeX) (fromIntegral y * pixelSizeY)) (V2 pixelSizeX pixelSizeY)

draw :: AppST ()
draw = do
  renderer <- gets renderer_
  window <- gets window_
  machine <- gets machine_

  liftIO $ mapFrameBuffer (\x y p -> when p $ drawPixel renderer window x y) machine

appLoop :: AppST ()
appLoop = do
  events <- SDL.pollEvents
  let escPressed = flip any events $ \e -> case SDL.eventPayload e of
        SDL.KeyboardEvent keyboardEvent ->
          SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
            && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
        SDL.WindowClosedEvent _ -> True
        _ -> False

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

  evalStateT appLoop $
    AppState window renderer $
      appendProgram
        abcCode
        defaultMachine

  SDL.destroyWindow window
  return ()
