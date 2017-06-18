module Reconcilation.Client where

import System.IO
import System.IO.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import qualified SDL

import Common.State
import Common.Timed.State
import Common.Timed.Server
import Common.Server
import Common.Client

main :: IO ()
main = do
  handle <- connectToServer
  clientChan <- newChan
  serverChan <- newChan
  (window, renderer) <- initSDL

  mainLoop <- forkIO $ loop State{position=0} clientChan serverChan renderer 0 []
  runConnTimedClient handle clientChan serverChan

  killThread mainLoop
  quitSDL window renderer

fakeClientID :: Int
fakeClientID = 1

loop :: State -> Chan TimedClientEvent -> Chan TimedServerEvent -> SDL.Renderer -> Int -> [TimedClientEvent] -> IO ()
loop state clientChan serverChan renderer time events = do
  clientEvent <- pullClientEvent

  case clientEvent of
    Just c -> writeChan clientChan TimedClientEvent{clientPayload=c, clientTime=time + 1}
    Nothing -> return ()

  let clientTime' = case clientEvent of
        Just c -> time + 1
        Nothing -> time
      events' = case clientEvent of
        Just c -> TimedClientEvent{clientPayload=c, clientTime=time + 1} : events
        Nothing -> events
      state' :: State
      state' = case clientEvent of
        Just c -> updateState state $ ClientEvent{payload=c, clientID=fakeClientID}
        Nothing -> state

  isEmpty <- isEmptyChan serverChan
  if not isEmpty
    then do
      tEvent <- readChan serverChan
      case serverEvent tEvent of
        Sync state'' -> do
          let events'' = filter (\e -> clientTime e > clientTime2 tEvent) events'
              cEvents = map (\e -> ClientEvent{payload=clientPayload e, clientID=fakeClientID}) events''
              state''' = updateStateMulti state'' cEvents

          render renderer $ position state'''
          loop state''' clientChan serverChan renderer clientTime' events''
    else do
      render renderer $ position state
      loop state' clientChan serverChan renderer clientTime' events'

updateStateMulti :: State -> [ClientEvent] -> State
updateStateMulti state [] = state
updateStateMulti state (e:es) = updateStateMulti (updateState state e) es