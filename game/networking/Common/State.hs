{-# LANGUAGE
TemplateHaskell,
MultiParamTypeClasses,
FunctionalDependencies,
FlexibleInstances
  #-}
module Common.State where
import Control.Lens

data State = State { position :: Int
                   }
  deriving (Show, Read)

data ClientEventPayload =
    ClientClick Click
  | ClientQuit
  deriving (Read, Show)

data Click =
    RIGHT
  | LEFT
  deriving (Read, Show)

data ServerEvent =
    Sync State
  | ServerQuit
  deriving (Read, Show)

data ClientEvent = ClientEvent { _clientEventPayload :: ClientEventPayload
                               , _clientEventClientID :: Int
                               }
makeFields ''ClientEvent

data TimedClientEventPayload = TimedClientEventPayload { _timedClientEventPayloadPayload :: ClientEventPayload
                                                       , _timedClientEventPayloadClientTime :: Int
                                                       }
  deriving (Read, Show)
makeFields ''TimedClientEventPayload

data TimedClientEvent = TimedClientEvent { _timedClientEventPayload :: ClientEventPayload
                                         , _timedClientEventClientTime :: Int
                                         , _timedClientEventClientID :: Int
                                         }
  deriving (Read, Show)
makeFields ''TimedClientEvent

data TimedServerEvent = TimedServerEvent { _timedServerEventEvent :: ServerEvent
                                         , _timedServerEventClientTime :: Int
                                         }
  deriving (Read, Show)
makeFields ''TimedServerEvent

updateState :: State -> ClientEvent -> State
updateState state event = case event^.payload of
  ClientClick click -> case click of
    RIGHT -> State{position=position state + 1}
    LEFT -> State{position=position state - 1}
  ClientQuit -> state
