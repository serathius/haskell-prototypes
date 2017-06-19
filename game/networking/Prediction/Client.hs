{-# LANGUAGE LambdaCase #-}
module Prediction.Client where

import Network.Socket
import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Text.Read
import Data.Time.Clock
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
  clientUpdateChan <- dupChan clientChan
  serverChan <- newChan
  (window, renderer) <- initSDL

  stateVar <- atomically (newTVar State{position=0})
  serverUpdateThread <- forkIO $ serverUpdateStateLoop stateVar serverChan
  clientUpdateThread <- forkIO $ clientUpdateStateLoop stateVar clientUpdateChan
  renderThread <- forkIO $ renderLoop stateVar clientChan renderer
  runConnClient handle clientChan serverChan

  killThread serverUpdateThread
  killThread clientUpdateThread
  killThread renderThread
  quitSDL window renderer

serverUpdateStateLoop :: TVar State -> Chan ServerEvent -> IO ()
serverUpdateStateLoop stateVar serverChan = do
  event <- readChan serverChan
  case event of
    Sync state' -> do
      atomically $ do
        writeTVar stateVar $ state'
  serverUpdateStateLoop stateVar serverChan

clientUpdateStateLoop :: TVar State -> Chan ClientEventPayload -> IO ()
clientUpdateStateLoop stateVar clientChan = do
  event <- readChan clientChan
  atomically $ do
    state <- readTVar stateVar
    writeTVar stateVar $ updateState state ClientEvent{payload=event, clientID=1}
  clientUpdateStateLoop stateVar clientChan

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
