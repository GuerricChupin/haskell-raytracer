module Renderable ( Renderable
                  , hit
                  , contains
                  , firstIntersection
                  , normal
                  , colorAt
                  , reflectAt
                  ) where

import GeometricTypes (Ray, Point, Vector)
import Color

class Renderable a where
   hit :: Ray -> a -> Bool
   contains :: a -> Point -> Bool
   firstIntersection :: Ray -> a -> Maybe Point
   -- undefined result if the point is not on the object
   normal :: a -> Point -> Vector
   colorAt :: Point -> a -> Color
   reflectAt :: Point -> a -> Double
