module Common.Server where

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

import Common.State


data ServerData = ServerData { clientChan :: !(Chan ClientEvent)
                             , serverChan :: !(Chan ServerEvent)
                             , accepter :: !(ThreadId)
                             }

type ClientHandler = Socket -> Chan ClientEvent -> Chan ServerEvent -> Int -> IO ()
type ClientConnectionHandler = Handle -> Chan ServerEvent -> Chan ClientEvent -> Int -> IO ()

startServer :: PortNumber -> ClientConnectionHandler -> IO (ServerData)
startServer port connectionHandler = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 2
  clientChan <- newChan
  serverChan <- newChan
  clientAccepter <- forkIO $ acceptClients connectionHandler sock clientChan serverChan 0
  return ServerData{clientChan=clientChan, serverChan=serverChan, accepter=clientAccepter}

acceptClients :: ClientConnectionHandler -> ClientHandler
acceptClients connectionHandler sock readChan writeChan clientID = do
  (acceptedSocket, _) <- accept sock
  clientChan <- dupChan writeChan
  forkIO $ do
    handle <- sockToHandle acceptedSocket
    connectionHandler handle clientChan readChan clientID
  acceptClients connectionHandler sock readChan writeChan (clientID + 1)

sockToHandle :: Socket -> IO Handle
sockToHandle sock = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl

stopServer :: ServerData -> IO ()
stopServer server = do
  killThread (accepter server)

parseClientEventPayload :: String -> Maybe ClientEventPayload
parseClientEventPayload = readMaybe

runConnServer :: ClientConnectionHandler
runConnServer hdl sink source clientID = do
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
      case parseClientEventPayload input of
        Just payload -> do
          writeChan source ClientEvent{payload=payload, clientID=clientID}
        Nothing -> return ()
      loop

runConnClient :: Handle -> Chan ClientEventPayload -> Chan ServerEvent -> IO ()
runConnClient hdl sink source = do
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

