module Intersectable ( Intersectable
                     , hit
                     , contains
                     , firstIntersection
                     , boundingBox
                     , Direction (..)
                     , BoundingBox (..)
                     , bimapBB
                     ) where

import Geometry (Point, Vector, Ray)

import Data.Maybe (isJust)

-- whether a ray enters or leave a solid at an intersection point.
-- for non-closed objects (e.g. a plane), rays should always be Leaving.
data Direction = Entering | Leaving
   deriving (Eq)

data BoundingBox = BoundingBox { xmin :: Double
                               , ymin :: Double
                               , zmin :: Double
                               , xmax :: Double
                               , ymax :: Double
                               , zmax :: Double
                               }

bimapBB :: (Double -> Double -> Double) -> (Double -> Double -> Double) ->
           BoundingBox -> BoundingBox -> BoundingBox
bimapBB x y (BoundingBox a b c d e f) (BoundingBox g h i j k l) =
  BoundingBox (x a g) (x b h) (x c i) (y d j) (y e k) (y f l)

class Intersectable a where
   hit :: Ray -> a -> Bool
   hit r obj = isJust $ firstIntersection r obj
   contains :: a -> Point -> Bool
   firstIntersection :: Ray -> a -> Maybe ( Point
                                          , Vector
                                          , Direction
                                          )
   boundingBox :: a -> BoundingBox
