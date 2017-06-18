module LockStep.Client where

import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import qualified SDL

import Common.State
import Common.Server
import Common.Client

main :: IO ()
main = do
  handle <- connectToServer
  clientChan <- newChan
  serverChan <- newChan
  (window, renderer) <- initSDL

  mainLoop <- forkIO $ loop State{position=0} clientChan serverChan renderer
  runConnClient handle clientChan serverChan

  killThread mainLoop
  quitSDL window renderer

loop :: State -> Chan ClientEventPayload -> Chan ServerEvent -> SDL.Renderer -> IO ()
loop state clientChan serverChan renderer = do
  clientEvent <- pullClientEvent
  case clientEvent of
    Just c -> writeChan clientChan c
    Nothing -> return ()
  isEmpty <- isEmptyChan serverChan
  if not isEmpty
    then do
      event <- readChan serverChan
      case event of
        Sync state' -> do
          render renderer $ position state'
          loop state' clientChan serverChan renderer
    else do
      render renderer $ position state
      loop state clientChan serverChan renderer
