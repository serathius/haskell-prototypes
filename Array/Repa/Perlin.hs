module Repa.Perlin where

import Data.Word
import Numeric.Noise.Perlin
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.DevIL as RD
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

width :: Int
width = 1024
height :: Int
height = 1024


main :: IO ()
main = do
  d <- perlinFillArray 1.0
  c <- R.copyP d
  removeIfExists "result.jpg"
  RD.runIL $ RD.writeImage "result.jpg" $ RD.Grey c

perlinFillArray :: Double -> IO (R.Array R.U R.DIM2 Word8)
perlinFillArray freq = R.computeP c
  where
    noise :: Perlin
    noise = perlin 1 5 (2 * freq/ fromIntegral width) 0.5
    b = R.fromFunction ((R.Z R.:. width ) R.:. height) (\(R.Z R.:. x R.:. y) -> noiseValue noise (fromIntegral x, fromIntegral y, 0))
    c = R.map (\i -> round (((i + 1) / 2) * 255)) b

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e