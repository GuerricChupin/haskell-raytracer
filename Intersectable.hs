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

import Geometry (Point, Vector, Ray, rayPlaneIntersection)

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

intersectBB :: Ray -> BoundingBox -> Bool
intersectBB r (BoundingBox a b c d e f) =
   any (maybe False within) inters
   where
   inters = do
      orig <- [(a,b,c), (d,e,f)]
      n <- [(1,0,0), (0,1,0), (0,0,1)]
      return $ rayPlaneIntersection r orig n
   within (x,y,z) =
      a <= x && x <= d && b <= y && y <= e && c <= z && z <= f

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

