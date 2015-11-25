module Intersectable ( Intersectable
                     , hit
                     , contains
                     , getFirstIntersection
                     , firstIntersection
                     , boundingBox
                     , Direction (..)
                     , BoundingBox (..)
                     , bimapBB
                     , intersectBB
                     ) where

import Geometry (Point, Vector, Ray)

import Data.Maybe (isJust)

import Debug.Trace

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

intersectBB :: Ray -> BoundingBox -> Bool
intersectBB ((xo, yo, zo), (x, y, z), _) (BoundingBox a b c d e f) =
  if z /= 0
  then let xint = xo + k * x
           yint = yo + k * y
           k = (e - zo)/z in
       a <= xint && xint <= b && c <= yint && yint <= d
  else
    if y /= 0
    then let xint = xo + k * x
             k = (c - yo)/y in
         a <= xint && xint <= b && e <= zo && zo <= f
    else
      if x /= 0
      then c <= yo && yo <= d && e <= zo && zo <= f
      else False
              
class Intersectable a where
   hit :: Ray -> a -> Bool
   hit r obj = isJust $ firstIntersection r obj
   contains :: a -> Point -> Bool
   -- | Calculate ray/object intersection, without bounding box optimization.
   getFirstIntersection :: Ray -> a -> Maybe ( Point
                                             , Vector
                                             , Direction
                                             )
   boundingBox :: a -> BoundingBox

firstIntersection :: (Intersectable a)
                     => Ray -> a -> Maybe (Point, Vector, Direction)
firstIntersection r x
   | intersectBB r (boundingBox x) = getFirstIntersection r x
   | otherwise                     = Nothing

