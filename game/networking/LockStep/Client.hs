module LockStep.Client where

import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import qualified SDL
import Data.Word
import Control.Concurrent.STM

import Common.State
import Common.Server
import Common.Client

main :: IO ()
main = do
  handle <- connectToServer
  clientChan <- newChan
  serverChan <- newChan
  (window, renderer) <- initSDL

  stateVar <- atomically (newTVar State{position=0})
  updateThread <- forkIO $ updateStateLoop stateVar serverChan
  renderThread <- forkIO $ renderLoop stateVar clientChan renderer
  runConnClient handle clientChan serverChan

  killThread updateThread
  killThread renderThread
  quitSDL window renderer

updateStateLoop :: TVar State -> Chan ServerEvent -> IO ()
updateStateLoop stateVar serverChan = do
  event <- readChan serverChan
  case event of
    Sync state' -> do
      atomically $ do
        writeTVar stateVar state'
  updateStateLoop stateVar serverChan

renderLoop :: TVar State -> Chan ClientEventPayload -> SDL.Renderer -> IO ()
renderLoop stateVar clientChan renderer = do
  currentTick <- SDL.ticks
  renderLoop' stateVar clientChan renderer currentTick

renderLoop' :: TVar State -> Chan ClientEventPayload -> SDL.Renderer -> Word32 -> IO ()
renderLoop' stateVar clientChan renderer previousTick = do
  clientEvent <- pullClientEvent
  case clientEvent of
    Just c -> writeChan clientChan c
    Nothing -> return ()
  state <- readTVarIO stateVar
  render renderer $ position state
  currentTick <- waitForNextFrame previousTick
  renderLoop' stateVar clientChan renderer currentTick
