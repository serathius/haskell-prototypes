module Storable.Perlin where

import Control.Monad
import Data.Function
import Data.Word
import qualified Data.Vector.Storable.Mutable as V
import Numeric.Noise.Perlin

width :: Int
width = 1024
height :: Int
height = 1024


main :: IO ()
main = do
  d <- perlinFillArray 1.0
  return ()


perlinFillArray :: Double -> IO (V.IOVector Word8)
perlinFillArray freq = do
    d <- V.replicate (width * height * 3) 0
    replicateNTimes (width * height - 1) $ fillPixel d
    return d
  where
    noise :: Perlin
    noise = perlin 1 5 (2 * freq/ fromIntegral width) 0.5
    fillPixel :: V.IOVector Word8 -> Int -> IO ()
    fillPixel d i = do
        V.write d (i*3) value
        V.write d (i*3+1) value
        V.write d (i*3+2) value
        return ()
      where
        x :: Double
        x = fromIntegral (mod i width)
        y :: Double
        y = (fromIntegral i) / ( fromIntegral width)
        nValue :: Double
        nValue = noiseValue noise (x, y, 0)
        value :: Word8
        value = round (((nValue + 1) / 2) * 255)



replicateNTimes :: (Num a, Eq a) => a -> (a -> IO()) -> IO ()
replicateNTimes (-1) _ = return ()
replicateNTimes n action = do
  action n
  replicateNTimes (n - 1) action
