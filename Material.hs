{-# LANGUAGE MonadComprehensions #-}
module Material where

import Linear
import System.Random

import Ray
import Hittable
import Colour

data Material g v a c = Material { scatterMaterial :: g -> Ray v a -> HitRecord g v a c -> (g, Maybe (V3 c, Ray v a)) }

lambertian :: (RandomGen g, Random a, Traversable v, Applicative v, Metric v, Floating a) => V3 c -> Material g v a c
lambertian a = Material
  { scatterMaterial = \g r hr ->
    let (g', ruv) = randomUnitVector g
        sd = normal hr ^+^ ruv
        scattered = Ray (p hr) sd
    in (g', Just (a, scattered))
  }

metal :: (RandomGen g, Traversable v, Applicative v, Metric v, Random a, Ord a, Floating a) => V3 c -> a -> Material g v a c
metal a fuzz = Material $ \g r hr ->
  let
    reflected = signorm (dir r) `reflect` normal hr
    (g', fuzzV) = randomUnitVector g
    scattered = Ray (p hr) (reflected ^+^ fuzz *^ fuzzV)
  in (g', [(a, scattered) | dir scattered `dot` normal hr > 0])
