module LightSource ( LightSource (LightSource)
                   , direction
                   ) where
                   
import GeometricTypes

data LightSource = LightSource { direction :: Vector }

