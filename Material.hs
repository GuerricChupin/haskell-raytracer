module Material ( Material (Mat)
                , color
                , reflect
                , anyMat
                ) where

import Color

data Material = Mat { color :: Color
                    -- reflect factor between 0 and 1
                    , reflect :: Double
                    } deriving (Eq)

-- material with arbitrary characterics, typically to use when the material
-- is not important
anyMat :: Material
anyMat = Mat { color = black
                 , reflect = 0
                 }

