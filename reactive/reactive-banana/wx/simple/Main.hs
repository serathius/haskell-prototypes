module Main where
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

main :: IO ()
main = start $ do
  f  <- frame [text :="simple"]
  input <- entry f []
  output <- staticText f [color := red]

  set f [layout := margin 10 $ column 10 $ [widget input, minsize (sz 40 20) $ widget output]]
  let networkDescription :: MomentIO ()
      networkDescription = do
        inputText <- behaviorText input ""
        sink output [text:== inputText]
  network <- compile networkDescription
  actuate network
