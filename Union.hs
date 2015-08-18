module Union ( Union (Union)
             , (|||)
             ) where

import Renderable
import GeometricTypes
import AuxiliaryFunctions (minOn, distance)

data Union a b = Union a b

(|||) :: a -> b -> Union a b
a ||| b = Union a b

instance (Renderable a, Renderable b) => Renderable (Union a b) where
   hit r (Union a b) = hit r a || hit r b
   contains (Union a b) p = contains a p || contains b p
   firstIntersection r (Union a b) = case (m, m') of
      (Nothing, m') -> m'
      (m, Nothing)  -> m
      (Just i, Just i') -> Just $ minOn (distance (origin r) . point) i i'
      where m = firstIntersection r a
            m' = firstIntersection r b

