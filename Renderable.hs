module Renderable ( Renderable
                  , hit
                  , intersections
                  , normal
                  ) where

import GeometricTypes (Ray, Point)

class Renderable a where
   hit :: Ray -> a -> Bool
   intersections :: Ray -> a -> [Point]
   -- undefined result if the point is not on the object
   normal :: a -> Point -> Ray

