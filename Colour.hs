module Colour where

import Codec.Picture
import Linear
import Numeric.Interval.Kaucher

toColour :: (Floating a, RealFrac a) => Int -> V3 a -> PixelRGB8
toColour samplesPerPixel v = 
  let scale = 1 / fromIntegral samplesPerPixel
  in case fmap (floor . (*) 256 . clamp (0 ... 0.999) . sqrt . (*) scale) v of
       V3 x y z -> PixelRGB8 x y z
