module Shading ( Shader
               , materialAt
               , Uniform (Uniform)
               , mat
               , uniform
               , plain
               , ShadedObject (Shaded)
               ) where

import Material
import Renderable
import qualified Intersectable as I
import Color
import Geometry
import Data.Maybe (isNothing, fromJust)

class Shader s where
   materialAt :: Point -> s -> Material

newtype Uniform = Uniform { mat :: Material }

instance Shader Uniform where
   materialAt _ (Uniform m) = m

-- apply uniform material to an object
infixl 7 `uniform`
uniform :: a -> Material -> ShadedObject a Uniform
uniform obj mat = Shaded obj (Uniform mat)

-- apply a uniform colour to an object with no reflexion and no transparency.
infixl 7 `plain`
plain :: a -> Color -> ShadedObject a Uniform
plain o c = Shaded o (Uniform Mat { color = c, reflect = 0, refract = 1,
                                   opacity = 1 })

data ShadedObject o s = Shaded o s

instance (I.Intersectable o, Shader s) => Renderable (ShadedObject o s) where
   firstIntersection r (Shaded o s)
      | isNothing hit = Nothing
      | otherwise     = Just $
         IntersectInfo { point = p
                       , normal = n
                       , localMat = mat
                       , n2 = n2
                       }
         where hit = I.firstIntersection r o
               (p, n, direction) = fromJust hit
               mat = materialAt p s
               n2 = if direction == I.Entering
                    then refract mat
                    else outerRefr

instance (I.Intersectable o) => I.Intersectable (ShadedObject o s) where
   I.contains r (Shaded o _) = I.contains r o
   I.costlyFirstIntersection r (Shaded o _) = I.costlyFirstIntersection r o

