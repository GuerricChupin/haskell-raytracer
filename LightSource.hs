module LightSource ( LightSource (LightSource)
                   , direction
                   ) where
                   
import Geometry (Vector)

data LightSource = LightSource { direction :: Vector }

