module Main where

import Reactive.Banana
import Reactive.Banana.SDL2
import Reactive.Banana.Frameworks
import qualified SDL as SDL
import Linear
import Data.Text
import Foreign.C.Types
import Linear.Affine

data State = State { position :: V2 CInt}

main :: IO ()
main = do
  (window, renderer) <- initialize
  eventSource <- getSDLEventSource
  network <- compile (describeNetwork renderer eventSource)
  actuate network
  runSDLPump eventSource
  quit window renderer

initialize :: IO (SDL.Window, SDL.Renderer)
initialize = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (pack "Render") SDL.defaultWindow
  renderer <- configureRenderer window
  return (window, renderer)

configureRenderer :: SDL.Window -> IO SDL.Renderer
configureRenderer window = do
  let rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True
                                     }
  SDL.createRenderer window (-1) rdrConfig

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

update :: SDL.Keysym -> State -> State
update k (State (V2 x y) ) = case SDL.keysymScancode k of
  SDL.ScancodeUp -> State (V2 x (y - 10))
  SDL.ScancodeDown -> State (V2 x (y + 10))
  SDL.ScancodeLeft -> State (V2 (x - 10) y)
  SDL.ScancodeRight -> State (V2 (x + 10) y)
  _ -> State (V2 x y)

render :: SDL.Renderer -> State -> IO ()
render renderer s = do
  SDL.clear renderer
  SDL.drawRect renderer (Just $ SDL.Rectangle (P $ position s) (V2 10 10))
  SDL.present renderer

describeNetwork :: SDL.Renderer -> SDLEventSource -> MomentIO ()
describeNetwork renderer eventSource = do
  tick <- tickEvent eventSource
  events <- sdlEvent eventSource
  pos <- accumE (State (V2 0 0)) (update <$> (keyDownEvent events))
  posB <- stepper (State (V2 0 0)) pos
  reactimate $ fmap (render renderer) (posB <@ tick)
  return ()
