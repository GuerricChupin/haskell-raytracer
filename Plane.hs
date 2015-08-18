module Plane ( Plane (Plane)
             , Plane.origin
             , normal
             , mat
             ) where

import GeometricTypes (Point, Vector, Ray (..))
import qualified GeometricTypes as G
import qualified Renderable as R
import AuxiliaryFunctions
import Material

epsilon :: Double
epsilon = 1.0e-12

data Plane = Plane { origin :: Point
                   , normal :: Vector
                   , mat :: Material
                   }

instance R.Renderable Plane where
  hit Ray { G.origin = o, G.dir = u } (Plane p n _) = ((o .- p) `dotProd` n) * (u `dotProd` n) < 0
  contains _ _ = False
  firstIntersection Ray { G.origin = o, G.dir = u } (Plane p n _)
      | t < epsilon     = Nothing
      | otherwise = Just $ o .+ (t .* u)
      where t = -((o .- p) `dotProd` n) / (u `dotProd` n)
  normal Plane { normal = n } _ = n
  colorAt _ Plane { mat = m } = color m
  reflectAt _ Plane { mat = m } = reflect m
  opacityAt _ Plane { mat = m } = opacity m
  refractAt _ Plane { mat = m } = refract m
