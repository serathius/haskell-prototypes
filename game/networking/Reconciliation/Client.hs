module Reconcilation.Client where

import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import qualified SDL
import Data.Word
import Control.Concurrent.STM

import Common.State
import Common.Timed.State
import Common.Timed.Server
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
  eventsVar <- atomically (newTVar [])
  serverUpdateThread <- forkIO $ serverUpdateStateLoop stateVar eventsVar serverChan
  clientUpdateThread <- forkIO $ clientUpdateStateLoop stateVar eventsVar clientUpdateChan
  renderThread <- forkIO $ renderLoop stateVar clientChan renderer
  runConnTimedClient handle clientChan serverChan

  killThread serverUpdateThread
  killThread clientUpdateThread
  killThread renderThread
  quitSDL window renderer

fakeClientID :: Int
fakeClientID = 1

serverUpdateStateLoop :: TVar State -> TVar [TimedClientEventPayload] -> Chan TimedServerEvent -> IO ()
serverUpdateStateLoop stateVar eventsVar serverChan = do
  tEvent <- readChan serverChan
  case serverEvent tEvent of
    Sync state -> do
      atomically $ do
        events <- readTVar eventsVar
        let events' = filter (\e -> clientTime e > clientTime2 tEvent) events
            cEvents = map (\e -> ClientEvent{payload=clientPayload e, clientID=fakeClientID}) events'
            state' = updateStateMulti state cEvents
        writeTVar eventsVar events'
        writeTVar stateVar state'
  serverUpdateStateLoop stateVar eventsVar serverChan

clientUpdateStateLoop :: TVar State -> TVar [TimedClientEventPayload] -> Chan TimedClientEventPayload -> IO ()
clientUpdateStateLoop stateVar eventsVar clientChan = do
  event <- readChan clientChan
  atomically $ do
    state <- readTVar stateVar
    events <- readTVar eventsVar
    writeTVar stateVar $ updateState state ClientEvent{payload=clientPayload event, clientID=1}
    writeTVar eventsVar $ event:events
  clientUpdateStateLoop stateVar eventsVar clientChan

renderLoop :: TVar State -> Chan TimedClientEventPayload -> SDL.Renderer -> IO ()
renderLoop stateVar clientChan renderer = do
  currentTick <- SDL.ticks
  renderLoop' stateVar clientChan renderer currentTick 0

renderLoop' :: TVar State -> Chan TimedClientEventPayload -> SDL.Renderer -> Word32 -> Int -> IO ()
renderLoop' stateVar clientChan renderer previousTick clientTime = do
  clientEvent <- pullClientEvent
  case clientEvent of
    Just c -> writeChan clientChan TimedClientEventPayload{clientPayload=c, clientTime=clientTime}
    Nothing -> return ()
  let clientTime' = case clientEvent of
        Just _ -> clientTime + 1
        Nothing -> clientTime
  state <- readTVarIO stateVar
  render renderer $ position state
  currentTick <- waitForNextFrame previousTick
  renderLoop' stateVar clientChan renderer currentTick clientTime'

updateStateMulti :: State -> [ClientEvent] -> State
updateStateMulti state [] = state
updateStateMulti state (e:es) = updateStateMulti (updateState state e) es