module Chessboard ( Chessboard (Chessboard)
                  , chessboardShaded
                  ) where

import Shading
import Plane (Plane)
import qualified Plane as P
import Geometry hiding (origin)
import Material
import Debug.Trace

-- planar chessboard shader for plane objects
data Chessboard = Chessboard { origin     :: Point
                             , uZ         :: Vector
                             , uX         :: Vector
                             , squareSize :: Double
                             , mat1       :: Material
                             , mat2       :: Material
                             }

instance Shader Chessboard where
   materialAt p (Chessboard { origin = o
                            , uZ = uZ
                            , uX = uX
                            , squareSize = sqSize
                            , mat1 = mat1
                            , mat2 = mat2
                            }) =
      if f x == f y
      then mat1
      else mat2
      where f x = floor (x / sqSize) `mod` 2
            (x, y) = planeCoords o uZ uX p

-- returns a point's coordinates in a plane's basis.
-- undefined if the point is not on the plane.
-- 'direction' and 'uZ' must be normalised and distinct.
planeCoords :: Point -> Vector -> Vector -> Point -> (Double, Double)
planeCoords origin uZ direction p = (op `dotProd` uX, op `dotProd` uY)
   where op = p .- origin
         uX = normalise $ uY `crossProd` uZ
         uY = normalise $ uZ `crossProd` direction

-- commodity function to apply a Chessboard shader to a plane.
chessboardShaded :: Plane -> Vector -> Double -> Material -> Material
                 -> ShadedObject Plane Chessboard
chessboardShaded plane uX sqSize m1 m2 =
   Shaded plane (Chessboard { origin = P.origin plane
                            , uZ = normalise $ P.normal plane
                            , uX = uX
                            , squareSize = sqSize
                            , mat1 = m1
                            , mat2 = m2
                            })

