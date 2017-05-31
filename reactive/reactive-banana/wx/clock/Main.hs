module Main where

import Data.Time
import Control.Concurrent
import Control.Monad
import Graphics.UI.WX hiding (Event, newEvent)
import Reactive.Banana
import Reactive.Banana.Frameworks (fromPoll)
import Reactive.Banana.WX



main :: IO ()
main = start $ do
  f <- frame [text := "wx-clock"]
  t <- timer f [interval := 1000]
  output <- staticText f [color := red]

  set f [layout := margin 10 $ column 10 $ [minsize (sz 40 20) $ widget output]]
  let networkDescription :: MomentIO ()
      networkDescription = do
        eTick <- event0 t command
        bTime <- fromPoll getCurrentTime
        let
          eTime :: Event UTCTime
          eTime = bTime <@ eTick
          timeStr :: Event String
          timeStr = fmap show eTime
        outputText <- stepper "" timeStr
        sink output [text:== outputText]
  network <- compile networkDescription
  actuate network

