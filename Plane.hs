module Plane ( Plane (Plane)
             , Plane.origin
             , normal
             , mat
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
import qualified Renderable as R
import Material

epsilon :: Double
epsilon = 1.0e-12

data Plane s = Plane { origin :: Point
                     , normal :: Vector
                     , mat :: Material s
                     }

instance (Shader s) => R.Renderable (Plane s) where
   hit Ray { G.origin = o, G.dir = u } (Plane p n _) =
      ((o .- p) `dotProd` n) * (u `dotProd` n) < 0
   contains _ _ = False
   firstIntersection Ray { G.origin = o, G.dir = u } (Plane p n mat)
      | t < epsilon     = Nothing
      | otherwise = Just $ R.IntersectInfo { R.point = o .+ (t .* u)
                                           , R.normal = n
                                           , R.localMat = mat
                                           , R.n2 = R.outerRefr
                                           }
      where t = -((o .- p) `dotProd` n) / (u `dotProd` n)

