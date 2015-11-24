module Renderable ( Renderable
                  , IntersectInfo (IntersectInfo)
                  , point
                  , normal
                  , localMat
                  , n2
                  , outerRefr
                  , firstIntersection
                  ) where

import Geometry (Ray, Point, Vector)
import Material

-- Type storing the characteristics of a ray-object intersection
data IntersectInfo = IntersectInfo { point :: Point
                                   , normal :: Vector
                                   , localMat :: Material
                                   -- refraction index of the medium being
                                   -- entered in
                                   , n2 :: Double
                                   } deriving (Show)

class Renderable a where
   -- | Calculates ray/object intersection and return intersection info, with
   -- bounding box optimization.
   firstIntersection :: Ray -> a -> Maybe IntersectInfo
