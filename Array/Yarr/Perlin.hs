module Yarr.Perlin where

import Data.Word
import qualified Data.Yarr as Y
import Numeric.Noise.Perlin

width :: Int
width = 1024
height :: Int
height = 1024

main :: IO ()
main = do
  a <- perlinArray 1.0
  return ()

perlinArray :: Double -> IO (Y.UArray Y.F Y.L Y.Dim2 Word8)
perlinArray freq = Y.dComputeP b
  where
    noise :: Perlin
    noise = perlin 1 5 (2 * freq / fromIntegral width) 0.5
    perlinPixel :: (Int, Int) -> IO Double
    perlinPixel (x, y) = do
      return $ noiseValue noise (fromIntegral x, fromIntegral y, 0)
    a = Y.fromFunction (width, height) perlinPixel
    b = Y.dmap (\i -> round (((i + 1) / 2) * 255)) a

