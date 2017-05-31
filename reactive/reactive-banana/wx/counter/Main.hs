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
  f <- frame [text := "counter"]
  b <- button f [text := "Tick"]
  output <- staticText f [color := red]
  set f [layout := margin 10 $ column 10 $ [widget b, minsize (sz 40 20) $ widget output]]
  let networkDescription :: MomentIO ()
      networkDescription = do
        click <- event0 b command
        counter <- accumE 0 $ (+1) <$ click
        outputText <- stepper "0" (fmap show counter)
        sink output [text:== outputText]
  network <- compile networkDescription
  actuate network

