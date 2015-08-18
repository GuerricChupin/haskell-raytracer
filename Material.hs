module Material ( Material (Mat)
                , color
                , reflect
                , refract
                , opacity
                ) where

import Color

data Material = Mat { color :: Color
                    -- reflect factor between 0 and 1
                    , reflect :: Double
                    -- positive coefficient
                    , refract :: Double
                    -- coefficient between 0 and 1
                    , opacity :: Double
                    } deriving (Eq)

-- material with arbitrary characterics, typically to use when the material
-- is not important
defaultMat :: Material
defaultMat = Mat { color = black
                 , reflect = 0
                 , refract = 1
                 , opacity = 1
                 }

