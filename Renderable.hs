module Renderable ( Renderable
                  , IntersectInfo (IntersectInfo)
                  , point
                  , normal
                  , localMat
                  , hit
                  , contains
                  , firstIntersection
                  ) where

import GeometricTypes (Ray, Point, Vector)
import Material

-- Type storing the characteristics of a ray-object intersection
data IntersectInfo = IntersectInfo { point :: Point
                                   , normal :: Vector
                                   , localMat :: Material
                                   -- refraction index of the new medium
                                   , n2 :: Double
                                   }

class Renderable a where
   hit :: Ray -> a -> Bool
   contains :: a -> Point -> Bool
   firstIntersection :: Ray -> a -> Maybe IntersectInfo

