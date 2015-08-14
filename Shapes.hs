module Shapes where 

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)

data Ray = Ray { origin :: Point
               , dir :: Vector
               }           
  
data Sphere = Sphere { center :: Point
                     , radius :: Double
                     }
              
