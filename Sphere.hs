{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Sphere where

import Linear
import Linear.Affine
import Numeric.Interval.Kaucher

import Hittable
import Ray
import Material

data Sphere g v a c = Sphere
  { centre :: Point v a
  , radius :: a
  , sphereMat :: Material g v a c
  }

instance (Metric v, Ord a, Floating a) => Hittable g v a c (Sphere g v a c) where
  hit s range r =
    let oc = orig r .-. centre s
        a = quadrance (dir r)
        halfb = oc `dot` dir r
        c = quadrance oc - radius s * radius s
        discriminant = halfb * halfb - a * c
        root = sqrt discriminant
        mat = sphereMat s
    in case () of
    _ | discriminant > 0, let t = (negate halfb - root) / a, t `member` range ->
      let p = projectRay t r
          normal = (p .-. centre s) ^/ radius s
      in Just HitRecord {..}
    _ | discriminant > 0, let t = (negate halfb + root) / a, t `member` range ->
      let p = projectRay t r
          normal = (p .-. centre s) ^/ radius s
      in Just HitRecord {..}
    _ | otherwise -> Nothing
