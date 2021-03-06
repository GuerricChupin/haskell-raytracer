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

main = sequence_ [i >>= writeFile ("film/film" ++ show j ++ ".ppm") . show |
        (i,j) <- zip (map (\c -> render c scene) camera) [0..]]
       -- render (1366 * 2, 768 * 2) (27.32, 15.36) 40 scene >>= putStr . show

n = 50

camera :: [Camera]
camera = [fixedAspectRatio (pi/4) $
          Camera (floor $ 1366*0.25, floor $ 768*0.25) (0,25*cos (i/n *2*pi),-25*sin (i/n*2*pi)) (0,-25*cos (i/n * 2*pi),25*sin(i/n *2*pi)) 0 (0,0) 10| i <- [0..n]]

spheres :: [Object]
spheres =    [Object $ Sphere (0,3,0) 3 `uniform` Mat (255,0,0) 0.1 1.4 0.1]
          ++ [Object $ Sphere (-5,3,5) 3 `uniform` Mat (0,255,0) 0.1 1.4 0.1]
          ++ [Object $ Sphere (7,3,-3) 3 `uniform` Mat (0,0,255) 0.1 1.4 0.1]
          ++ [Object $ chessboardShaded (Plane (0,0,0) (0,1,0)) (1,0,0) 3
                       (Mat (255,255,255) 0 1 1)
                       (Mat (0,0,0) 0 1 1)]
scene = mkScene
   LightSource { direction = (1,1,1) }
   spheres

rotV = G.rotateVect (1,0,0) (-pi/12)
rotP = G.rotatePt (0,0,-5) (1,0,0) (-pi/12)
