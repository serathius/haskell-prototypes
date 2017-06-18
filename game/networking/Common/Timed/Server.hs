module Common.Timed.Server where

import Network.Socket
import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Fix (fix)
import Text.Read
import Control.Concurrent.STM

import Common.Timed.State
import Common.Server
import Common.State

parseTimedClientEvent :: String -> Maybe TimedClientEvent
parseTimedClientEvent = readMaybe

runConnTimedServer :: ClientConnectionHandler
runConnTimedServer hdl sink source clientID = do
  timeVar <- atomically (newTVar 0)
  runConnTimedServer' hdl sink source clientID timeVar

runConnTimedServer' :: Handle -> Chan ServerEvent -> Chan ClientEvent -> Int -> TVar Int -> IO ()
runConnTimedServer' hdl sink source clientID timeVar = do
    reader <- forkIO fromSinkToHandle
    fromHandleToSource
    killThread reader
    hClose hdl
  where
    fromSinkToHandle = fix $ \loop -> do
      event <- readChan sink
      time <- readTVarIO timeVar
      hPutStrLn hdl $ show TimedServerEvent{serverEvent=event, clientTime2=time}
      loop
    fromHandleToSource = fix $ \loop -> do
      input <- hGetLine hdl
      case parseTimedClientEvent input of
        Just tEvent -> do
          atomically $ do
            writeTVar timeVar $ clientTime tEvent
          writeChan source $ ClientEvent{payload=clientPayload tEvent, clientID=clientID}
        Nothing -> return ()
      loop

runConnTimedClient :: Handle -> Chan TimedClientEvent -> Chan TimedServerEvent -> IO ()
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
