module AuxiliaryFunctions ( (.+)
                          , (.-)
                          , neg
                          , (.*)
                          , dotProd
                          , crossProd
                          , sqNorm
                          , norm
                          , normalise
                          , distance
                          , sym
                          ) where

import GeometricTypes
import Color

(.+) :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
(a,b,c) .+ (d,e,f) = (a+d,b+e,c+f)

(.-) :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
(a,b,c) .- (d,e,f) = (a-d,b-e,c-f)

neg :: (Num a) => (a,a,a) -> (a,a,a)
neg (a,b,c) = (negate a, negate b, negate c)

(.*) :: Double -> Vector -> Vector
n .* (a,b,c) = (n*a, n*b, n*c)

dotProd :: Vector -> Vector -> Double
dotProd (x,y,z) (x',y',z') = x*x' + y*y' + z*z'

crossProd :: Vector -> Vector -> Vector
crossProd (a,b,c) (d,e,f) = (x,y,z)
  where x = b*f - c*e
        y = c*d - a*f
        z = a*e - b*d

sqNorm :: Vector -> Double
sqNorm x = dotProd x x

norm :: Vector -> Double
norm = sqrt . sqNorm

normalise :: Vector -> Vector
normalise x = (1/norm x) .* x

distance :: Point -> Point -> Double
distance a b = norm (b .- a)

sym :: Vector -> Vector -> Vector
sym u n = (2 * u `dotProd` n) .* normalise n .- u
