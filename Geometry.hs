module Geometry ( Point
                , Vector
                , Ray (Ray)
                , origin
                , dir
                , refr
                , (.+)
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
                , rotateVect
                , rotatePt
                ) where

import Debug.Trace

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)

data Ray = Ray { origin :: Point
               , dir :: Vector
               -- refraction index in the current medium
               , refr :: Double
               } 

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
normalise x | n == 0    = (0, 0, 0)
            | otherwise = (1/n) .* x
            where n = norm x

distance :: Point -> Point -> Double
distance a b = norm (b .- a)

sym :: Vector -> Vector -> Vector
sym u n = (2 * u `dotProd` n) .* normalise n .- u

rotateVect :: Vector -> Double -> Vector -> Vector
rotateVect axis angle u = axisProj .+ rotOrthProj
   where axisProj = (u `dotProd` uZ) .* uZ
         uZ = normalise axis
         uY = normalise $ uZ `crossProd` u
         uX = uY `crossProd` uZ
         rotOrthProj = (u `dotProd` uX) .* ((cos angle .* uX) .+ (sin angle .* uY))

rotatePt :: Point -> Vector -> Double -> Point -> Point
rotatePt origin axis angle = (origin .+) . (rotateVect axis angle) . (.- origin)

