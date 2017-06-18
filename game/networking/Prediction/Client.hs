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

import Common.Server
import Common.State
import Common.Client

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  connect sock $ SockAddrInet (4242 :: PortNumber) $ tupleToHostAddress (127, 0, 0, 1)
  handle <- sockToHandle sock
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
  let state' :: State
      state' = case clientEvent of
        Just c -> updateState state $ ClientEvent{payload=c, clientID=1}
        Nothing -> state
  isEmpty <- isEmptyChan serverChan
  if not isEmpty
    then do
      event <- readChan serverChan
      case event of
        Sync state'' -> do
          render renderer $ position state''
          loop state'' clientChan serverChan renderer
    else do
      render renderer $ position state'
      loop state' clientChan serverChan renderer
