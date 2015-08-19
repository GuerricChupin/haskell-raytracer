module AuxiliaryFunctions ( minOn
                          ) where

minOn :: (Ord b) => (a -> b) -> a -> a -> a
minOn f a b = case compare (f a) (f b) of
   LT -> a
   GT -> b
   EQ -> a

