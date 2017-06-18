module Common.State where

data State = State { position :: Int
                   }
  deriving (Show, Read)

data ClientEvent = ClientEvent { payload :: ClientEventPayload
                               , clientID :: Int
                               }

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

updateState :: State -> ClientEvent -> State
updateState s e = case payload e of
  ClientClick click -> case click of
    RIGHT -> State{position=position s + 1}
    LEFT -> State{position=position s - 1}
  ClientQuit -> s
