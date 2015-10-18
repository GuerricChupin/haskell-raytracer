{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies, FlexibleInstances #-}



module Geometry ( Point
                , Vector
                , Ray
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

import qualified Data.Array.Accelerate as A

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)

-- Rays are composed of an origin and a direction. They also save the
-- refraction index in the current medium.
type Ray = (Point, Vector, Double)

origin :: A.Exp Ray -> A.Exp Point
origin ray = o
  where (o,_,_) = A.unlift ray :: (A.Exp Point, A.Exp Vector, A.Exp Double)

dir :: A.Exp Ray -> A.Exp Vector
dir ray = d
  where (_, d, _) = A.unlift ray :: (A.Exp Point, A.Exp Vector, A.Exp Double)

refr :: A.Exp Ray -> A.Exp Double
refr ray = r
  where (_, _, r) = A.unlift ray :: (A.Exp Point, A.Exp Vector, A.Exp Double)

(.+) :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
(a,b,c) .+ (d,e,f) = (a+d,b+e,c+f)

(.-) :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
(a,b,c) .- (d,e,f) = (a-d,b-e,c-f)

neg :: (Num a) => (a,a,a) -> (a,a,a)
neg (a,b,c) = (negate a, negate b, negate c)

(.*) :: (Num a) => a -> (a,a,a) -> (a,a,a)
n .* (a,b,c) = (n*a, n*b, n*c)

dotProd :: (Num a) => (a,a,a) -> (a,a,a) -> a
dotProd (x,y,z) (x',y',z') = x*x' + y*y' + z*z'

crossProd :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
crossProd (a,b,c) (d,e,f) = (x,y,z)
  where x = b*f - c*e
        y = c*d - a*f
        z = a*e - b*d

sqNorm :: (Num a) => (a,a,a) -> a
sqNorm x = dotProd x x

norm :: (Floating a) => (a,a,a) -> a
norm = sqrt . sqNorm

normalise :: (Floating a, Eq a) => (a,a,a) -> (a,a,a)
normalise x | n == 0    = (0, 0, 0)
            | otherwise = (1/n) .* x
            where n = norm x

distance :: (Floating a) => (a,a,a) -> (a,a,a) -> a
distance a b = norm (b .- a)

sym :: (Floating a, Eq a) => (a,a,a) -> (a,a,a) -> (a,a,a)
sym u n = (2 * u `dotProd` n) .* normalise n .- u

rotateVect :: (Floating a, Eq a) => (a,a,a) -> a -> (a,a,a) -> (a,a,a) 
rotateVect axis angle u = axisProj .+ rotOrthProj
   where axisProj = (u `dotProd` uZ) .* uZ
         uZ = normalise axis
         uY = normalise $ uZ `crossProd` u
         uX = uY `crossProd` uZ
         rotOrthProj = (u `dotProd` uX) .* ((cos angle .* uX) .+ (sin angle .* uY))

rotatePt :: (Floating a, Eq a) => (a,a,a) -> (a,a,a) -> a -> (a,a,a) -> (a,a,a)
rotatePt origin axis angle = (origin .+) . (rotateVect axis angle) . (.- origin)

