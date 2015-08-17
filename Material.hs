module Material ( Material (Mat)
                , color
                , reflect
                ) where

import Color

data Material = Mat { color :: Color
                    -- reflect factor between 0 and 1
                    , reflect :: Double
                    } deriving (Eq)

-- material with arbitrary characterics, typically to use when the material
-- is not important
defaultMat :: Material
defaultMat = Mat { color = black
                 , reflect = 0
                 }

