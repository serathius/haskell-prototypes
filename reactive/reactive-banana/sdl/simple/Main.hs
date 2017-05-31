module Main where

import Reactive.Banana
import Reactive.Banana.SDL2
import Reactive.Banana.Frameworks
import qualified SDL as SDL
import Linear
import Data.Text


main :: IO ()
main = do
  window <- initialize
  fillWhite window
  eventSource <- getSDLEventSource
  network <- compile $ describeNetwork eventSource window
  actuate network
  runSDLPump eventSource
  quit window

initialize :: IO SDL.Window
initialize = do
  SDL.initialize [SDL.InitVideo]
  SDL.createWindow (pack "Render") SDL.defaultWindow

quit :: SDL.Window -> IO ()
quit window = do
  SDL.destroyWindow window
  SDL.quit

describeNetwork :: SDLEventSource -> SDL.Window -> MomentIO ()
describeNetwork eventSource window = do
  tick <- tickEvent eventSource
  reactimate $ fmap (const $ SDL.updateWindowSurface window) tick
  return ()

fillWhite :: SDL.Window -> IO ()
fillWhite window = do
  surface <- SDL.getWindowSurface window
  let white = V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect surface Nothing white
  SDL.freeSurface surface
  return ()
