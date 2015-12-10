module AuxiliaryFunctions ( minOn
                          , sgn
                          ) where

minOn :: (Ord b) => (a -> b) -> a -> a -> a
minOn f a b = case compare (f a) (f b) of
   LT -> a
   GT -> b
   EQ -> a

sgn :: (Ord a, Num a) => a -> a
sgn n = if n >= 0 then 1 else (-1)
