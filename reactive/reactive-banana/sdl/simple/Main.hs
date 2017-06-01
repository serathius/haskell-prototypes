module Main where

import Reactive.Banana
import Reactive.Banana.SDL2
import Reactive.Banana.Frameworks
import qualified SDL as SDL
import Linear

white = V4 maxBound maxBound maxBound maxBound

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Reactive-Banana SDL Simple" SDL.defaultWindow

  eventSource <- getSDLEventSource
  let describeNetwork :: MomentIO ()
      describeNetwork = do
        tick <- tickEvent eventSource
        reactimate $ fmap (const $ fillWhite window) tick
  network <- compile describeNetwork
  actuate network
  runSDLPump eventSource

  SDL.destroyWindow window
  SDL.quit

fillWhite :: SDL.Window -> IO ()
fillWhite window = do
  surface <- SDL.getWindowSurface window
  SDL.surfaceFillRect surface Nothing white
  SDL.freeSurface surface
  SDL.updateWindowSurface window
