import Scene
import Render
import Sphere
import LightSource
import Material
import Plane
import Shading
import Chessboard (chessboardShaded)
import qualified Geometry as G
import Color (black, white)

main =
  putStr . show $ render (1366 `div` 3, 768 `div` 3) (27.32, 15.36) 40 scene -- std for testing
  -- putStr . show $ render (1366, 768) (27.32, 15.36) 40 scene -- bigger

scene = Scene {
     world =   Sphere (0, 0, -10) 5 `uniform` Mat (200, 0, 0) 1 1 1
        {-
           ||| chessboardShaded Plane { origin = (0, -5, -10)
                                      , normal = (0, 1, 0)
                                      }
                                (1, 1, 1) 2 (Mat white 1 1 1) (Mat black 1 1 1)
                                -}

   , source = LightSource { direction = (1, 1, 1) }
   }

