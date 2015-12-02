module Plane ( Plane (Plane)
             , Plane.origin
             , normal
             ) where

import Geometry ( Point
                , Vector
                , Ray
                , dotProd
                , (.+)
                , (.-)
                , (.*)
                )
import qualified Geometry as G
import Intersectable

data Plane = Plane { origin :: Point
                   , normal :: Vector
                   }

instance Intersectable Plane where
   hit (o, u, _) (Plane p n) =
      ((o .- p) `dotProd` n) * (u `dotProd` n) < 0
   contains _ _ = False
   firstIntersection r (Plane p n) =
      fmap (\i -> (i, n, Leaving)) $ G.rayPlaneIntersection r p n
