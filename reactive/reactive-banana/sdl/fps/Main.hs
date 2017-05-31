module Main where

import Reactive.Banana
import Reactive.Banana.SDL2
import Reactive.Banana.Frameworks
import qualified SDL as SDL
import Linear
import qualified SDL.TTF as SDL.TTF
import qualified SDL.Raw as SDL.Raw
import SDL.TTF.Types
import SDL.TTF.FFI
import Data.Text
import Data.Word


main :: IO ()
main = do
  (window, renderer, font) <- initialize
  eventSource <- getSDLEventSource
  network <- compile $ describeNetwork renderer eventSource font
  actuate network
  runSDLPump eventSource
  Main.quit window renderer

initialize :: IO (SDL.Window, SDL.Renderer, TTFFont)
initialize = do
  SDL.initialize [SDL.InitVideo]
  SDL.TTF.init
  window <- SDL.createWindow (pack "Render") SDL.defaultWindow
  font <- SDL.TTF.openFont "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" 13
  renderer <- configureRenderer window
  return (window, renderer, font)

configureRenderer :: SDL.Window -> IO SDL.Renderer
configureRenderer window = do
  let rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True
                                     }
  SDL.createRenderer window (-1) rdrConfig

render :: SDL.Renderer -> TTFFont -> Double -> IO ()
render renderer font fps = do
    SDL.clear renderer
    texture <- renderText font renderer $ "fps: " ++ (show $ cutDigits 1 fps)
    SDL.copy renderer texture Nothing Nothing
    SDL.destroyTexture texture
    SDL.present renderer

describeNetwork :: SDL.Renderer -> SDLEventSource -> TTFFont -> MomentIO ()
describeNetwork renderer eventSource font = do
  tick <- tickEvent eventSource
  ticks <- groupE 120 tick
  let fps :: Event Double
      fps = fmap calculateFps ticks
  reactimate $ fmap (render renderer font) fps
  return ()

cutDigits n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

calculateFps :: [Word32] -> Double
calculateFps (f:[]) = realToFrac f
calculateFps (f:fs) = 1000 * (realToFrac $ Prelude.length fs) / realToFrac (f - Prelude.last fs)

renderText :: TTFFont -> SDL.Renderer -> String -> IO SDL.Texture
renderText font renderer text = do
  surface <- SDL.TTF.renderTextSolid font text (SDL.Raw.Color 255 255 255 0)
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return texture

groupE :: MonadMoment m => Int -> Event a -> m (Event [a])
groupE n a = do
  let lastTwo :: [c] -> [c] -> [c]
      lastTwo (a:[]) bs = Prelude.take n (a:bs)
  accumE [] $ lastTwo . (\b -> [b]) <$> a

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.TTF.quit
  SDL.quit

