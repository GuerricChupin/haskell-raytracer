module Plane ( Plane (Plane)
             , Plane.origin
             , normal
             ) where

import Geometry ( Point
                , Vector
                , Ray (..)
                , dotProd
                , (.+)
                , (.-)
                , (.*)
                )
import qualified Geometry as G
import Intersectable

epsilon :: Double
epsilon = 1.0e-12

data Plane = Plane { origin :: Point
                   , normal :: Vector
                   }

instance Intersectable Plane where
   hit (o, u, _) (Plane p n) =
      ((o .- p) `dotProd` n) * (u `dotProd` n) < 0
   contains _ _ = False
   firstIntersection (o, u, _) (Plane p n)
      | t < epsilon || un == 0 = Nothing
      | otherwise = Just $ (o .+ (t .* u), n, Leaving)
      where t = -((o .- p) `dotProd` n) / un
            un = u `dotProd` n

