module Camera where

import Linear
import Linear.Affine

import Ray

data Camera v w a = Camera
  { cameraOrig :: Point v a
  , lowerLeftCorner :: Point v a
  , offset :: w (v a)
  }

-- a default camera setting
camera :: Camera V3 V2 Double
camera = 
  let aspectRatio = 16 / 9
      viewportHeight = 2.0
      viewportWidth = aspectRatio * viewportHeight
      focalLength = 1
      offset = V2 (V3 viewportWidth 0 0) (V3 0 viewportHeight 0)
  in Camera
    { cameraOrig = P (V3 0 0 0)
    , lowerLeftCorner = origin .-^ V2 0.5 0.5 *! offset .-^ V3 0 0 focalLength
    , offset = offset
    }

getRay :: (Additive v, Foldable w, Additive w, Num a) => Camera v w a -> w a -> Ray v a
getRay c v = Ray 
  { orig = cameraOrig c
  , dir = lowerLeftCorner c .+^ v *! offset c .-. cameraOrig c
  }
