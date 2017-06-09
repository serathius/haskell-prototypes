module Repa.Stacking where

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
  a <- stackPerlin
  saveJpgImage 100 "result.jpg" $ ImageY8 $ toImage a


toImage :: R.Array R.U R.DIM2 Word8 -> Image Pixel8
toImage a = generateImage gen width height
  where
    gen x y =  a R.! (R.Z R.:. x R.:. y)

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
