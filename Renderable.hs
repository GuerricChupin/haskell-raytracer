module Renderable ( Renderable
                  , IntersectInfo (IntersectInfo)
                  , point
                  , normal
                  , localMat
                  , n2
                  , outerRefr
                  , hit
                  , contains
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
                                   }

-- refraction index of the outer space
outerRefr :: Double
outerRefr = 1

class Renderable a where
   hit :: Ray -> a -> Bool
   contains :: a -> Point -> Bool
   firstIntersection :: Ray -> a -> Maybe IntersectInfo

