module Renderable ( Renderable
                  , hit
                  , contains
                  , intersections
                  , normal
                  , colorAt
                  , reflectAt
                  ) where

import GeometricTypes (Ray, Point)
import Color

class Renderable a where
   hit :: Ray -> a -> Bool
   contains :: a -> Point -> Bool
   intersections :: Ray -> a -> [Point]
   -- undefined result if the point is not on the object
   normal :: a -> Point -> Ray
   colorAt :: Point -> a -> Color
   reflectAt :: Point -> a -> Double
