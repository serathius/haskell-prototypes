module Common.Timed.State where
import Common.State

data TimedClientEvent = TimedClientEvent { clientPayload :: ClientEventPayload
                                         , clientTime :: Int
                                         }
  deriving (Read, Show)

data TimedServerEvent = TimedServerEvent { serverEvent :: ServerEvent
                                         , clientTime2 :: Int
                                         }
  deriving (Read, Show)
