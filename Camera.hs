module Camera ( Camera(..)
              , fixedAspectRatio
              ) where

import Geometry

data Camera = Camera { resolution :: (Int, Int)
                     , centreScreenPosition :: Point
                     , orientation :: Vector
                     , flip :: Double
                     , openings :: (Double, Double)
                     , toScreenDistance :: Double
                     }

fixedAspectRatio :: Double -> Camera -> Camera
fixedAspectRatio d c@Camera { resolution = (w, h) } =
  c { openings = (d, (ar * d)) }
  where
    -- I have no idea why this fraction is not the other way roud.
    ar = fromIntegral w / fromIntegral h
