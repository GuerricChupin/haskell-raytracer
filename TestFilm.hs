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
import Camera

import Debug.Trace

main = sequence_ [i >>= writeFile ("film/film" ++ show j ++ ".ppm") . show |
        (i,j) <- zip (map (\c -> render c scene) camera) [0..]]
       -- render (1366 * 2, 768 * 2) (27.32, 15.36) 40 scene >>= putStr . show

n = 100

camera :: [Camera]
camera = [fixedAspectRatio (pi/4) $
          Camera (floor $ 1366*0.5, floor $ 768*0.5) (0,25*sin (i/n *pi),-25* cos (i/n*pi)) (0,-25*sin (i/n * pi),25*cos(i/n *pi)) 0 (0,0) 10| i <- [0..n]]

spheres :: [Object]
spheres =    [Object $ Sphere (0,0,0) 3 `uniform` Mat (255,0,0) 0.1 1.4 0.5]
          ++ [Object $ Sphere (-5,0,5) 3 `uniform` Mat (0,255,0) 0.1 1.4 0.5]
          ++ [Object $ Sphere (7,0,-3) 3 `uniform` Mat (0,0,255) 0.1 1.4 0.5]
          ++ [Object $ chessboardShaded (Plane (0,-3,0) (0,1,0)) (1,0,0) 3
                       (Mat (255,255,255) 0.5 1 1)
                       (Mat (0,0,0) 0.5 1 1)]
scene = mkScene
   LightSource { direction = (1,1,1) }
   spheres

rotV = G.rotateVect (1,0,0) (-pi/12)
rotP = G.rotatePt (0,0,-5) (1,0,0) (-pi/12)
