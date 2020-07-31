{-# LANGUAGE NamedFieldPuns #-}
module Ray where

import Control.Applicative
import Data.Traversable
import Linear
import Linear.Affine
import System.Random

data Ray v a = Ray
  { orig :: Point v a
  , dir  :: v a
  }

-- | Project a 'Ray' by the given distance
projectRay :: (Additive v, Num a) => a -> Ray v a -> Point v a
projectRay t Ray{orig, dir} = orig .+^ t *^ dir

-- vector utils
-- Some of these should be added to 

randomInUnitSphere :: (RandomGen g, Random (v a), Applicative v, Metric v, Ord a, Num a) => g -> (g, v a)
randomInUnitSphere g = 
  let (v, g') = randomR (pure (-1), pure 1) g
  in if quadrance v <= 1 then (g', v) else randomInUnitSphere g'

randomUnitVector :: (RandomGen g, Random a, Traversable v, Applicative v, Metric v, Floating a) => g -> (g, v a)
randomUnitVector g =
  -- not really Gaussian but close
  let randomGaussian g0 = 
        let (a, g1) = random g0
            (b, g2) = random g1
            (c, g3) = random g2
            (d, g4) = random g3
            (e, g5) = random g4
            (f, g6) = random g5
            (g, g7) = random g6
            (h, g8) = random g7
            (i, g9) = random g8
            (j, g10) = random g9
            (k, g11) = random g10
            (l, g12) = random g11
        in (g12, sum [a,b,c,d,e,f,g,h,i,j,k,l] - 6)
  in fmap signorm $ mapAccumR (const . randomGaussian) g (pure 0)

reflect :: (Metric v, Num a) => v a -> v a -> v a
reflect v n = v ^-^ 2 * v `dot` n *^ n

refract :: (Metric v, Floating a) => v a -> v a -> a -> v a
refract uv n ee =
  let cosTheta = - uv `dot` n
      rPerp = ee *^ (uv ^+^ cosTheta *^ n)
      rPare = (- sqrt (abs (1 - quadrance rPerp))) *^ n
  in rPerp ^+^ rPare

(^*^) :: (Applicative v, Num a) => v a -> v a -> v a
(^*^) = liftA2 (*)
