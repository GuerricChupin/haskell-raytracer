module Intersection ( Intersection (Inter)
                    ) where

import Intersectable
import Geometry
import Data.Function (on)

data Intersection a b = Inter a b

instance (Intersectable a, Intersectable b) =>
         Intersectable (Intersection a b) where
   contains (Inter a b) p = contains a p && contains b p
   getFirstIntersection r int@(Inter a b) =
     case (firstIntersection r a, firstIntersection r b) of
      (Nothing, Nothing) -> Nothing
      (Nothing, m'@(Just (p,_,_))) -> if a `contains` p then m' else Nothing
      (m@(Just (p,_,_)), Nothing) -> if b `contains` p then m else Nothing
      (i@(Just (p,_,_)), i'@(Just (p',_,_))) ->
        case (compare `on` distance (origin r)) p p' of
         LT -> if b `contains` p
               then i
               else firstIntersection (p, dir r, refr r) int
         _  -> if a `contains` p'
               then i'
               else firstIntersection (p', dir r, refr r) int
   boundingBox (Inter a b) = bimapBB max min (boundingBox a) (boundingBox b)
