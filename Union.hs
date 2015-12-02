module Union ( Union (Union)
             , (|||)
             ) where

import Renderable
import Geometry
import AuxiliaryFunctions (minOn)
import Material
import qualified Intersectable as I

data Union a b = Union a b

infixl 6 |||
(|||) :: a -> b -> Union a b
a ||| b = Union a b

instance (I.Intersectable a, I.Intersectable b)
   => I.Intersectable (Union a b) where
   contains (Union a b) p = I.contains a p || I.contains b p
   firstIntersection r (Union a b) = case (m, m') of
      (Nothing, m') -> m'
      (m, Nothing)  -> m
      (Just i, Just i') -> Just $ minOn (distance o . pt) i i'
      where m = I.firstIntersection r a
            m' = I.firstIntersection r b
            o = origin r
            pt (a,_,_) = a
