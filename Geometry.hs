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
                , rayPlaneIntersection
                , epsilon
                , Axis (..)
                , axisOrientedPlaneHit
                ) where

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)

-- Rays are composed of an origin and a direction. They also save the
-- refraction index in the current medium.
type Ray = (Point, Vector, Double)

origin :: Ray -> Point
origin (o, _, _) = o

dir :: Ray -> Vector
dir (_, d, _) = d

refr :: Ray -> Double
refr (_, _, r) = r

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

epsilon :: Double
epsilon = 1.0e-11

-- | Returns the intersection (if any) between a ray and a plane. If the ray
-- origin is on the plane (with precision 'epsilon'), then returns 'Nothing'.
rayPlaneIntersection :: Ray -> Point -> Vector -> Maybe Point
rayPlaneIntersection (o, u, _) p n
   | t < epsilon || un == 0 = Nothing
   | otherwise = Just $ o .+ (t .* u)
   where t = -((o .- p) `dotProd` n) / un
         un = u `dotProd` n

data Axis = XAxis | YAxis | ZAxis

axisOrientedPlaneHit :: Ray -> Point -> Axis -> Maybe Point
axisOrientedPlaneHit (o@(xo,_,_), d@(xd,_,_), _) (x,_,_) XAxis
   | xd * (x-xo) > 0 = Just $ o .+ (((x - xo)/xd) .* d)
   | otherwise  = Nothing
axisOrientedPlaneHit (o@(_,yo,_), d@(_,yd,_), _) (_,y,_) YAxis
   | yd * (y-yo) > 0 = Just $ o .+ (((y - yo)/yd) .* d)
   | otherwise  = Nothing
axisOrientedPlaneHit (o@(_,_,zo), d@(_,_,zd), _) (_,_,z) ZAxis
   | zd * (z-zo) > 0 = Just $ o .+ (((z - zo)/zd) .* d)
   | otherwise  = Nothing

