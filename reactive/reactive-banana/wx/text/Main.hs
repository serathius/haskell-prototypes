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
  f <- frame [text := "text"]
  input <- entry f [text := ""]
  b <- button f [text := "Write"]
  output <- staticText f [color := red]
  set f [layout := margin 10 $ column 10 $ [widget input, widget b, widget output]]
  let networkDescription :: MomentIO ()
      networkDescription = do
        click <- event0 b command
        newLine <- behaviorText input ""
        let
          joinLines :: String -> String -> String
          joinLines a b = if b == ""
            then a
            else if a == ""
              then b
              else a ++ "\n" ++ b
          a :: Behavior (String -> String)
          a = fmap (flip joinLines) newLine
          addLine :: Event (String -> String)
          addLine = a <@ click
        counter <- accumE "" addLine
        outputText <- stepper "" counter
        sink output [text:== outputText]
  network <- compile networkDescription
  actuate network

