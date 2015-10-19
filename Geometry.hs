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

origin :: (a,b,c) -> a
origin (o,_,_) = o

dir :: (a,b,c) -> b
dir (_, d, _) = d

refr :: (a,b,c) -> c
refr (_, _, r) = r

(.+) :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
(a,b,c) .+ (d,e,f) = (a+d,b+e,c+f)

(.-) :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
(a,b,c) .- (d,e,f) = (a-d,b-e,c-f)

neg :: (Num a) => (a,a,a) -> (a,a,a)
neg (a,b,c) = (negate a, negate b, negate c)

(.*) :: (Num a) => a -> (a,a,a) -> (a,a,a)
n .* (a,b,c) = (n*a, n*b, n*c)

--dotProd :: (Num a) => (a,a,a) -> (a,a,a) -> a
dotProd :: A.Exp Vector -> A.Exp Vector -> A.Exp Double
dotProd a b = x*x' + y*y' + z*z'
  where (x,y,z) = A.unlift a :: (A.Exp Double, A.Exp Double, A.Exp Double)
        (x',y',z') = A.unlift b :: (A.Exp Double, A.Exp Double, A.Exp Double)

crossProd :: A.Exp Vector -> A.Exp Vector -> A.Exp Vector
crossProd u v = A.lift (x,y,z)
  where
    (a,b,c) = A.unlift u :: (A.Exp Double, A.Exp Double, A.Exp Double)
    (d,e,f) = A.unlift v :: (A.Exp Double, A.Exp Double, A.Exp Double)
    x = b*f - c*e
    y = c*d - a*f
    z = a*e - b*d

sqNorm :: A.Exp Vector -> A.Exp Double
sqNorm x = dotProd x x

norm :: A.Exp Vector -> A.Exp Double
norm = sqrt . sqNorm

normalise :: A.Exp Vector -> A.Exp Vector
normalise x | n == 0    = A.constant (0, 0, 0)
            | otherwise = (A.lift) $ (1/n) .* y
  where
    y = A.unlift x :: (A.Exp Double, A.Exp Double, A.Exp Double)
    n = norm x

distance :: A.Exp Vector -> A.Exp Vector -> A.Exp Double
distance a b = norm (A.lift $ d .- c)
  where c = A.unlift a :: (A.Exp Double, A.Exp Double, A.Exp Double)
        d = A.unlift b :: (A.Exp Double, A.Exp Double, A.Exp Double)

sym :: A.Exp Vector -> A.Exp Vector -> A.Exp Vector
sym u n = A.lift ((2 * u `dotProd` n) .*
                  (A.unlift $ normalise n) .-
                  (A.unlift u))
    
rotateVect :: A.Exp Vector -> A.Exp Double -> A.Exp Vector -> A.Exp Vector
rotateVect axis angle u = A.lift $ axisProj .+ rotOrthProj
   where axisProj = (u `dotProd` uZ) .* (A.unlift uZ)
         uZ = normalise axis
         uY = normalise $ uZ `crossProd` u
         uX = uY `crossProd` uZ
         rotOrthProj = (u `dotProd` uX) .*
                       ((cos angle .* (A.unlift uX)) .+
                        (sin angle .* (A.unlift uY)))

rotatePt :: A.Exp Point -> A.Exp Vector -> A.Exp Double -> A.Exp Vector
         -> A.Exp Vector
rotatePt origin axis angle =
  A.lift . (origin'.+) . (A.unlift) . (rotateVect axis angle) . (A.lift) . (.- origin') . A.unlift
  where
    origin' = A.unlift origin :: (A.Exp Double, A.Exp Double, A.Exp Double)
