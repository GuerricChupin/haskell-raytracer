{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Material ( Material (Mat)
                , color
                , reflect
                , refract
                , opacity
                , anyMat
                , outerRefr
                ) where

import Color
import Geometry (Point)

import qualified Data.Array.Accelerate as A

-- describes the rendering characteristics of an object at a given point.
data Material = Mat { color :: Color
                    -- reflect factor between 0 and 1
                    , reflect :: Double
                    -- positive coefficient
                    , refract :: Double
                    -- coefficient between 0 and 1
                    , opacity :: Double
                    } deriving (Eq, Show)

-- material with arbitrary characterics, typically to use when the material
-- is not important
anyMat :: Material
anyMat = Mat { color = black
             , reflect = 0
             , refract = 1
             , opacity = 1
             }

-- refraction index of the outer space
outerRefr :: Double
outerRefr = 1

