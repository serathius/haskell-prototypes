module Repa.Perlin where

import Data.Word
import Numeric.Noise.Perlin
import qualified Data.Array.Repa as R
import Codec.Picture (Image, Pixel8, generateImage, saveJpgImage, DynamicImage(ImageY8))

width :: Int
width = 1024
height :: Int
height = 1024


main :: IO ()
main = do
  a <- perlinFillArray 1.0
  saveJpgImage 100 "result.jpg" $ ImageY8 $ toImage a


toImage :: R.Array R.U R.DIM2 Word8 -> Image Pixel8
toImage a = generateImage gen width height
  where
    gen x y =  a R.! (R.Z R.:. x R.:. y)


perlinFillArray :: Double -> IO (R.Array R.U R.DIM2 Word8)
perlinFillArray freq = R.computeP c
  where
    noise :: Perlin
    noise = perlin 1 5 (2 * freq/ fromIntegral width) 0.5
    b = R.fromFunction ((R.Z R.:. width ) R.:. height) (\(R.Z R.:. x R.:. y) -> noiseValue noise (fromIntegral x, fromIntegral y, 0))
    c = R.map (\i -> round (((i + 1) / 2) * 255)) b
