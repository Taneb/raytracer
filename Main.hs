{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Codec.Picture
import Control.Lens hiding ((...))
import Data.Maybe
import Data.Traversable
import Linear
import Linear.Affine
import Numeric.Interval.Kaucher (Interval, (...), inf, member)
import System.Random

import Ray
import Colour
import Hittable
import Sphere
import Camera
import Material

rayColour :: (RandomGen g, Random a, Hittable g V3 a a s, Floating a, RealFrac a) => g -> Int -> s -> Ray V3 a -> (g, V3 a)
rayColour g depth world r@Ray{dir} 
  | depth <= 0 = (g, V3 0 0 0)
  | Just hr <- hit world (0.001 ... (1/0)) r = case scatterMaterial (mat hr) g r hr of
    (g', Nothing) -> (g', V3 0 0 0)
    (g', Just (a, s)) -> fmap (a ^*^) (rayColour g' (depth - 1) world s)
  | otherwise =
  let unitDir = signorm dir
      t = 0.5 * (unitDir ^. _y + 1)
  in (g, (1 - t) *^ V3 1 1 1 ^+^ t *^ V3 0.5 0.7 1.0)

world :: [SomeHittable StdGen V3 Double Double]
world =
  [ SomeHittable Sphere { centre = P (V3 0 0 (-1)), radius = 0.5, sphereMat = centre }
  , SomeHittable Sphere { centre = P (V3 0 (-100.5) (-1)), radius = 100, sphereMat = ground }
  , SomeHittable Sphere { centre = P (V3 (-1) 0 (-1)), radius = 0.5, sphereMat = left }
  , SomeHittable Sphere { centre = P (V3 1 0 (-1)), radius = 0.5, sphereMat = right }
  ]
  where
    ground = lambertian (V3 0.8 0.8 0)
    centre = lambertian (V3 0.7 0.3 0.3)
    left   = metal      (V3 0.8 0.8 0.8) 0.3
    right  = metal      (V3 0.8 0.6 0.2) 1

-- image stuff
aspectRatio :: Rational
aspectRatio = 16 / 9

width, height :: Int
width  = 400
height = floor (fromIntegral width / aspectRatio)

samplesPerPixel :: Int
samplesPerPixel = 100

maxDepth :: Int
maxDepth = 50

image :: Image PixelRGB8
image = snd $ generateFoldImage gen (mkStdGen 0) width height
  where
    gen0 :: StdGen -> Int -> Int -> (StdGen, V3 Double)
    gen0 g i j =
      let (xoff, g1) = random g
          (yoff, g2) = random g1
          u = (fromIntegral i + xoff) / (fromIntegral width  - 1)
          v = 1 - (fromIntegral j + yoff) / (fromIntegral height - 1) -- we want the y axis going up, not down
          r = getRay camera (V2 u v)
      in rayColour g2 maxDepth world r

    gen :: StdGen -> Int -> Int -> (StdGen, PixelRGB8)
    gen g i j = fmap (toColour samplesPerPixel . sum) $ mapAccumR (uncurry . gen0) g $ replicate samplesPerPixel (i, j)


main :: IO ()
main = writePng "out.png" image
