module Reconcilation.Server where

import Network.Socket
import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Text.Read
import Data.Time.Clock
import Network.Socket
import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import Text.Read
import Control.Lens

import Common.State
import Common.Server
import Common.Timed.State hiding (ClientEvent)
import Common.Timed.Server

main :: IO ()
main = do
  server <- startServer 4242 runConnTimedServer
  currentTime <- getCurrentTime
  let state = State{position=0}
  writeChan (serverChan server) $ TimedServerEvent (Sync state) 0
  loop state (clientChan server) (serverChan server) currentTime
  stopServer server

loop :: State -> Chan TimedClientEvent -> Chan TimedServerEvent -> UTCTime -> IO ()
loop state clientChan serverChan lastSyncTime = do
  tEvent <- readChan clientChan
  let state' = updateState state $ ClientEvent (tEvent^.payload) (tEvent^.clientID)
  currentTime <- getCurrentTime
  if realToFrac (diffUTCTime currentTime lastSyncTime) > 0.05
    then do
      print state'
      writeChan serverChan $ TimedServerEvent (Sync state') (tEvent^.clientTime)
      loop state' clientChan serverChan currentTime
    else
      loop state' clientChan serverChan lastSyncTime
