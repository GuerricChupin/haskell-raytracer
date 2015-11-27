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

import Geometry

import Data.Maybe (isJust, fromJust)

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
      (isJust mzmin && a <= zminx && zminx <= d && b <= zminy && zminy <= e)
   || (isJust mymin && a <= yminx && yminx <= d && c <= yminz && yminz <= f)
   || (isJust mxmin && b <= xminy && xminy <= e && c <= xminz && xminz <= f)
   || (isJust mzmax && a <= zmaxx && zmaxx <= d && b <= zmaxy && zmaxy <= e)
   || (isJust mymax && a <= ymaxx && ymaxx <= d && c <= ymaxz && ymaxz <= f)
   || (isJust mxmax && b <= xmaxy && xmaxy <= e && c <= xmaxz && xmaxz <= f)
   where mzmin = axisOrientedPlaneHit r (a,b,c) ZAxis
         (zminx, zminy, _) = fromJust mzmin
         mymin = axisOrientedPlaneHit r (a,b,c) YAxis
         (yminx, _, yminz) = fromJust mymin
         mxmin = axisOrientedPlaneHit r (a,b,c) XAxis
         (_, xminy, xminz) = fromJust mxmin
         mzmax = axisOrientedPlaneHit r (d,e,f) ZAxis
         (zmaxx, zmaxy, _) = fromJust mzmax
         mymax = axisOrientedPlaneHit r (d,e,f) YAxis
         (ymaxx, _, ymaxz) = fromJust mymax
         mxmax = axisOrientedPlaneHit r (d,e,f) XAxis
         (_, xmaxy, xmaxz) = fromJust mxmax

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

