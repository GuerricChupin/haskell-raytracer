module GeometricTypes ( Point
                      , Vector
                      , Ray (Ray)
                      , origin
                      , dir
                      ) where

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)

data Ray = Ray { origin :: Point
               , dir :: Vector
               } 
