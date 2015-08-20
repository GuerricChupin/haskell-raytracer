module Union ( Union (Union)
             , (|||)
             ) where

import Renderable
import Geometry
import AuxiliaryFunctions (minOn)
import Material

data Union a b = Union a b

infixl 6 |||
(|||) :: a -> b -> Union a b
a ||| b = Union a b

instance (Renderable a, Renderable b) => Renderable (Union a b) where
   firstIntersection r (Union a b) = case (m, m') of
      (Nothing, m') -> m'
      (m, Nothing)  -> m
      (Just i, Just i') -> Just $ let a = minOn (distance (origin r) . point) i i' in a
      where m = firstIntersection r a
            m' = firstIntersection r b
            o = origin r

