{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Hittable where

import Data.Maybe
import Linear
import Linear.Affine
import Numeric.Interval.Kaucher

import Ray
import {-# SOURCE #-} Material

data HitRecord g v a c = HitRecord
  { p :: Point v a
  , normal :: v a
  , t :: a
  , mat :: Material g v a c
  }

class Hittable g v a c s | s -> g, s -> v, s -> a, s -> c where
  hit :: s -> Interval a -> Ray v a -> Maybe (HitRecord g v a c)

data SomeHittable g v a c = forall s. Hittable g v a c s => SomeHittable s

instance Hittable g v a c (SomeHittable g v a c) where
  hit (SomeHittable s) range r = hit s range r

instance {-# OVERLAPPABLE #-} (Foldable f, Ord a, Hittable g v a c s) => Hittable g v a c (f s) where
  hit s range r = foldr check Nothing s
    where
      check s' Nothing = hit s' range r
      check s' (Just hr) = Just $ hr `fromMaybe` hit s' (inf range ... t hr) r
