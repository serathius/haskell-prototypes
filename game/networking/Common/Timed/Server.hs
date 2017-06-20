module Common.Timed.Server where

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

import Common.Timed.State
import Common.Server
import Common.State

parseTimedClientEventPayload :: String -> Maybe TimedClientEventPayload
parseTimedClientEventPayload = readMaybe

runConnTimedServer :: ClientConnectionHandler TimedServerEvent TimedClientEvent
runConnTimedServer hdl sink source clientID = do
    reader <- forkIO fromSinkToHandle
    fromHandleToSource
    killThread reader
    hClose hdl
  where
    fromSinkToHandle = fix $ \loop -> do
      event <- readChan sink
      hPutStrLn hdl $ show event
      loop
    fromHandleToSource = fix $ \loop -> do
      input <- hGetLine hdl
      case parseTimedClientEventPayload input of
        Just tEvent -> do
          writeChan source $ TimedClientEvent (tEvent^.payload) (tEvent^.clientTime) clientID
        Nothing -> return ()
      loop

runConnTimedClient :: Handle -> Chan TimedClientEventPayload -> Chan TimedServerEvent -> IO ()
runConnTimedClient hdl sink source = do
    reader <- forkIO fromSinkToHandle
    fromHandleToSource
    killThread reader
    hClose hdl
  where
    fromSinkToHandle = fix $ \loop -> do
      event <- readChan sink
      hPutStrLn hdl $ show event
      loop
    fromHandleToSource = fix $ \loop -> do
      input <- hGetLine hdl
      case readMaybe input of
        Just payload -> do
          writeChan source payload
        Nothing -> return ()
      loop
