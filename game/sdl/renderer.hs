module Renderer where

import Control.Monad
import Data.Function
import qualified Data.Text
import Foreign.C.Types
import qualified SDL
import Linear
import Linear.Affine (Point(P))

white :: V4 CInt
white = V4 maxBound maxBound maxBound maxBound

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Renderer" SDL.defaultWindow
  let rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True
                                     }
  renderer <- SDL.createRenderer window (-1) rdrConfig

  fix $ \loop -> do
    events <- SDL.pollEvents
    let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
    render renderer
    unless quit loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

render :: SDL.Renderer -> IO ()
render renderer = do
  SDL.clear renderer
  SDL.drawRect renderer (Just $ SDL.Rectangle (P $ V2 380 280) (V2 40 40))
  SDL.present renderer

