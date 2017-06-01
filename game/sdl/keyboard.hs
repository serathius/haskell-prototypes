{-# LANGUAGE LambdaCase #-}
module Keyboard where

import Control.Monad
import Data.Monoid
import Data.Maybe
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
  window <- SDL.createWindow "SDL Keyboard" SDL.defaultWindow
  let rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True
                                     }
  renderer <- SDL.createRenderer window (-1) rdrConfig

  loop renderer 0

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

loop :: SDL.Renderer -> CInt -> IO ()
loop renderer x = do
  events <- SDL.pollEvents
  let payloads = map SDL.eventPayload events
  let quit = elem SDL.QuitEvent payloads
  let x' = fromMaybe x $ getLast $
        foldMap (\case SDL.KeyboardEvent e
                         | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                           case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
                             SDL.KeycodeRight -> Last (Just $ x + 10)
                             SDL.KeycodeLeft  -> Last (Just $ x - 10)
                             _ -> mempty
                       _ -> mempty)
                payloads

  render renderer x'
  unless quit $ loop renderer x'

render :: SDL.Renderer -> CInt -> IO ()
render renderer x = do
  SDL.clear renderer
  SDL.drawRect renderer (Just $ SDL.Rectangle (P $ V2 (380 + x) 280) (V2 40 40))
  SDL.present renderer

