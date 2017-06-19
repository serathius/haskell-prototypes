module Common.Timed.State where
import Common.State

data TimedClientEventPayload = TimedClientEventPayload { clientPayload :: ClientEventPayload
                                                       , clientTime :: Int
                                                       }
  deriving (Read, Show)

data TimedClientEvent = TimedClientEvent { clientPayload2 :: ClientEventPayload
                                         , clientTime3 :: Int
                                         , clientID2 :: Int
                                         }
  deriving (Read, Show)

data TimedServerEvent = TimedServerEvent { serverEvent :: ServerEvent
                                         , clientTime2 :: Int
                                         }
  deriving (Read, Show)
