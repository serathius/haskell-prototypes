module Storable.Stacking where

import Control.Monad
import Data.Function
import Data.Word
import qualified Data.Vector.Storable.Mutable as V
import Numeric.Noise.Perlin

width :: Int
width = 512
height :: Int
height = 512

main :: IO ()
main = do
  d1 <- perlinFillArray 1.0
  d2 <- perlinFillArray 2.0
  d3 <- perlinFillArray 4.0
  d4 <- perlinFillArray 8.0
  d5 <- perlinFillArray 16.0
  d6 <- perlinFillArray 32.0
  d7 <- perlinFillArray 64.0
  d <- joinArrays [(d1, 64), (d2, 32), (d3, 16), (d4, 8), (d5, 4), (d6, 2), (d7, 1)]
  return ()

perlinFillArray :: Double -> IO (V.IOVector Double)
perlinFillArray freq = do
    d <- V.replicate (width * height) 0
    replicateNTimes (width * height - 1) $ fillPixel d
    return d
  where
    noise :: Perlin
    noise = perlin 1 5 (2 * freq/ fromIntegral width) 0.5
    fillPixel :: V.IOVector Double -> Int -> IO ()
    fillPixel d i = do
        V.write d i value
        return ()
      where
        x :: Double
        x = fromIntegral (mod i width)
        y :: Double
        y = (fromIntegral i) / ( fromIntegral width)
        value :: Double
        value = noiseValue noise (x, y, 0)

joinArrays :: [(V.IOVector Double, Double)] -> IO (V.IOVector Double)
joinArrays a = do
    new <- V.replicate (width * height) 0
    mapM ((flip addArray) new) a
    return new
  where
    weightSum :: Double
    weightSum = sum(map snd a)
    fillPixel :: V.IOVector Double -> V.IOVector Double -> Double -> Int -> IO ()
    fillPixel old new s i = do
      oldValue <- V.read old i
      currentValue <- V.read new i
      V.write new i $ currentValue + oldValue * s / weightSum
      return ()
    addArray :: (V.IOVector Double, Double) -> V.IOVector Double -> IO ()
    addArray (old, s) new = replicateNTimes (width * height - 1) $ fillPixel old new s

toBMP :: V.IOVector Double -> IO (V.IOVector Word8)
toBMP old = do
    new <- V.replicate (width * height * 3) 0
    replicateNTimes (width * height - 1) $ fillPixel old new
    return new
  where
    fillPixel :: V.IOVector Double -> V.IOVector Word8 -> Int -> IO ()
    fillPixel old new i = do
      oldValue <- V.read old i
      V.write new (i * 3) $ round (((oldValue+ 1) / 2) * 255)
      V.write new (i * 3 + 1) $ round (((oldValue+ 1) / 2) * 255)
      V.write new (i * 3 + 2) $ round (((oldValue+ 1) / 2) * 255)
      return ()

replicateNTimes :: (Num a, Eq a) => a -> (a -> IO()) -> IO ()
replicateNTimes (-1) _ = return ()
replicateNTimes n action = do
  action n
  replicateNTimes (n - 1) action
