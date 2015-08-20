{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Material ( Material (Mat)
                , shader
                , reflect
                , refract
                , opacity
                , Shader
                , colorAt
                , anyMat
                ) where

import Color
import Geometry (Point)

data Material a = Mat { shader :: a
                      -- reflect factor between 0 and 1
                      , reflect :: Double
                      -- positive coefficient
                      , refract :: Double
                      -- coefficient between 0 and 1
                      , opacity :: Double
                      } deriving (Eq)

class Shader a where
   -- undefined if point is not on the shaded object.
   colorAt :: Point -> a -> Color

instance Shader Color where
   colorAt _ c = c

-- material with arbitrary characterics, typically to use when the material
-- is not important
anyMat :: Material Color
anyMat = Mat { shader = black
             , reflect = 0
             , refract = 1
             , opacity = 1
             }

