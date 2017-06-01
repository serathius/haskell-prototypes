module Simple where

import Control.Monad
import Data.Function
import qualified Data.Text
import Foreign.C.Types
import qualified SDL
import Linear

white = V4 maxBound maxBound maxBound maxBound

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Simple" SDL.defaultWindow

  fix $ \loop -> do
    events <- SDL.pollEvents
    let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
    fillWhite window
    unless quit loop

  SDL.destroyWindow window
  SDL.quit

fillWhite :: SDL.Window -> IO ()
fillWhite window = do
  surface <- SDL.getWindowSurface window
  SDL.surfaceFillRect surface Nothing white
  SDL.freeSurface surface
  SDL.updateWindowSurface window
