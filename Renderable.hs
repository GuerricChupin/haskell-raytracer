module Renderable ( Renderable
                  , hit
                  ) where

import GeometricTypes (Ray)

class Renderable a where
   hit :: Ray -> a -> Bool

