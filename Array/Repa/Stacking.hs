module Repa.Stacking where

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
  a <- stackPerlin
  c <- R.copyP a
  removeIfExists "result.jpg"
  RD.runIL $ RD.writeImage "result.jpg" $ RD.Grey c


stackPerlin :: IO (R.Array R.U R.DIM2 Word8)
stackPerlin = R.computeP rounded
  where
    stacked =  R.map (/127) (
         (R.map (*64) $ perlinArray 1)
      R.+^ (R.map (*32) $ perlinArray 2)
      R.+^ (R.map (*16) $ perlinArray 4)
      R.+^ (R.map (*8) $ perlinArray 8)
      R.+^ (R.map (*4) $ perlinArray 16)
      R.+^ (R.map (*2) $ perlinArray 32)
      R.+^ (perlinArray 64)
      )
    rounded = R.map (\i -> round (((i + 1) / 2) * 255)) stacked

perlinArray :: Double -> R.Array R.D R.DIM2 Double
perlinArray freq = b
  where
    noise :: Perlin
    noise = perlin 1 5 (2 * freq/ fromIntegral width) 0.5
    b = R.fromFunction ((R.Z R.:. width ) R.:. height) (\(R.Z R.:. x R.:. y) -> noiseValue noise (fromIntegral x, fromIntegral y, 0))

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
