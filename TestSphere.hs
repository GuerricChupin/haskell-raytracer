import Scene
import Render
import Sphere
import LightSource
import Material
import Plane
import Shading
import Chessboard (chessboardShaded)
import Geometry ((.+), (.-))
import qualified Geometry as G
import Color (black, white)
import Intersection
import BiconvexLens
import Image

main = render (1366, 768) (27.32, 15.36) 40 scene >>= putStr . show
       -- render (1366 * 2, 768 * 2) (27.32, 15.36) 40 scene >>= putStr . show

spheres :: [Object]
spheres =    [Object $ Sphere (rotP (0,0,-15)) 3 `uniform` Mat (255,0,0) 0.1 1.4 0.5]
          ++ [Object $ Sphere (rotP (-5,0,-20)) 3 `uniform` Mat (0,255,0) 0.1 1.4 0.5]
          ++ [Object $ Sphere (rotP (7,0,-18)) 3 `uniform` Mat (0,0,255) 0.1 1.4 0.5]
          ++ [Object $ chessboardShaded (Plane (rotP (0,3,0)) (rotV (0,1,0))) (1,0,0) 3
                       (Mat (255,255,255) 0.5 1 1)
                       (Mat (0,0,0) 0.5 1 1)]
scene = mkScene
   LightSource { direction = rotV (1,1,1) }
   spheres

rotV = G.rotateVect (1,0,0) (-pi/12)
rotP = G.rotatePt (0,0,-5) (1,0,0) (-pi/12)
