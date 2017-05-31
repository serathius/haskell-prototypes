module Main where

import Data.Time
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

main :: IO ()
main = start $ do
  f  <- frame [text := "wx-mirror"]
  input <- entry f []
  output <- staticText f [color := red]

  set f [layout := margin 10 $ column 10 $ [widget input, minsize (sz 40 20) $ widget output]]
  let networkDescription :: MomentIO ()
      networkDescription = do
        inputText <- behaviorText input ""
        let
          outputText :: Behavior String
          outputText = fmap reverse inputText
        sink output [text:== outputText]
  network <- compile networkDescription
  actuate network

