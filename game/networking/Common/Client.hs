{-# LANGUAGE LambdaCase #-}
module Common.Client where

import System.IO
import Network.Socket
import Linear
import Linear.Affine (Point(P))
import Foreign.C.Types
import Data.Monoid
import Data.Maybe
import Data.Word
import Control.Monad

import Common.State
import qualified SDL

import Common.Server

fpsCap :: Word16
fpsCap = 60

secondsPerFrame = fromIntegral $ 1000 `div` fpsCap

connectToServer :: IO Handle
connectToServer = do
  sock <- socket AF_INET Stream 0
  connect sock $ SockAddrInet (4242 :: PortNumber) $ tupleToHostAddress (127, 0, 0, 1)
  sockToHandle sock

initSDL :: IO (SDL.Window, SDL.Renderer)
initSDL = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Keyboard" SDL.defaultWindow
  let rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True
                                     }
  renderer <- SDL.createRenderer window (-1) rdrConfig
  return (window, renderer)

quitSDL :: SDL.Window -> SDL.Renderer -> IO ()
quitSDL window renderer = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

render :: SDL.Renderer -> Int -> IO ()
render renderer x = do
  SDL.clear renderer
  SDL.drawRect renderer (Just $ SDL.Rectangle (P $ V2 (380 + CInt (fromIntegral x) * 10) 280) (V2 40 40))
  SDL.present renderer

pullClientEvent ::  IO (Maybe ClientEventPayload)
pullClientEvent = do
  events <- SDL.pollEvents
  let payloads = map SDL.eventPayload events
  let clientEvent :: Maybe ClientEventPayload
      clientEvent = getLast $
        foldMap (\case SDL.KeyboardEvent e
                         | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                           case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
                             SDL.KeycodeRight -> Last (Just $ ClientClick RIGHT)
                             SDL.KeycodeLeft  -> Last (Just $ ClientClick LEFT)
                             _ -> mempty
                       _ -> mempty)
                payloads
  return clientEvent

waitForNextFrame :: Word32 -> IO Word32
waitForNextFrame previousTick = do
  currentTick <- SDL.ticks
  let ticks = fromIntegral $ currentTick - previousTick
  when (ticks < secondsPerFrame) $ SDL.delay (secondsPerFrame - ticks)
  return currentTick
