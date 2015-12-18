module Sphere ( Sphere (Sphere)
              , center
              , radius
              , sphereIntersect
              ) where

import Intersectable
import Geometry
import Data.List ( minimumBy
                 )
import Data.Function (on)
import Data.Maybe (isJust)

data Sphere = Sphere { center  :: Point
                     , radius  :: Double
                     } deriving (Eq)

instance Intersectable Sphere where
   hit r s = isJust $ firstIntersection r s
   contains s p = distance p (center s) < radius s - epsilon
   firstIntersection (o, d, _) (Sphere c r)
      | dist > r  = Nothing
      | oc > r    = if tc > 0 then Just (p, p .- c, Entering)
                              else Nothing
      | otherwise = Just (p', p' .- c, Leaving)
   where du   = normalise d
         tc   = (c .- o) `dotProd` du
         dist = sqrt $ tc^2 - oc^2
         oc   = distance o c
         p    = o .+ ((tc - t1c) .* du)
         p'   = o .+ ((tc + t1c) .* du)
         t1c  = sqrt $ r^2 - dist^2

