module LockStep.Server where

import Network.Socket
import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Text.Read
import Data.Time.Clock

import Common.Server
import Common.State

main :: IO ()
main = do
  server <- startServer 4242 runConnServer
  currentTime <- getCurrentTime
  let state = State{position=0}
  writeChan (serverChan server) $ Sync state
  loop state (clientChan server) (serverChan server) currentTime
  stopServer server

loop :: State -> Chan ClientEvent -> Chan ServerEvent -> UTCTime -> IO ()
loop state clientChan serverChan lastSyncTime = do
  event <- readChan clientChan
  let state' = updateState state event
  currentTime <- getCurrentTime
  if realToFrac (diffUTCTime currentTime lastSyncTime) > 0.1
    then do
      print state'
      writeChan serverChan $ Sync state'
      loop state' clientChan serverChan currentTime
    else
      loop state' clientChan serverChan lastSyncTime
