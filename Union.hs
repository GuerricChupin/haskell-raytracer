module Union ( Union (Union)
             , (|||)
             ) where

import Renderable
import Geometry
import AuxiliaryFunctions (minOn)
import Material
import Debug.Trace
import Material (Shader)

data Union a s b s' = Union (a s) (b s')

(|||) :: a s -> b s' -> Union a s b s'
a ||| b = Union a b

instance (Renderable a, Renderable b, Shader s, Shader s')
      => Renderable (Union a b s s') where
   hit r (Union a b) = hit r a || hit r b
   contains (Union a b) p = contains a p || contains b p
   firstIntersection r (Union a b) = case (m, m') of
      (Nothing, m') -> m'
      (m, Nothing)  -> m
      (Just i, Just i') -> Just $ let a = minOn (distance (origin r) . point) i i' in a
      where m = firstIntersection r a
            m' = firstIntersection r b
            o = origin r

