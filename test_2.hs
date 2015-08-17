import Scene
import Render
import Sphere
import LightSource
import Difference
import AuxiliaryFunctions
import GeometricTypes

main = putStr . show $ render (400, 400) (20, 20) 40 scene

scene = Scene {
     objs = map MkSceneObject spheres ++
            --map MkSceneObject planes ++
            [MkSceneObject diff]
   , source = LightSource { direction = (1, 1, 1) }
   }

diff = Diff (MkSceneObject $ Sphere o 7 (255, 255, 255) 0.9)
            (MkSceneObject $ Sphere (o .+ rotateZY (angle) (0, 0, 20)) 20 (0, 255, 0) 0)

spheres = [ Sphere (o .+ rotateZY (angle) (0, 0, -3.5)) 1 (255, 0, 0) 0
          , Sphere (o .- (0, 109, 0)) 100 (255, 255, 0) 0
          ]
{-
planes = [ Plane { Plane.origin = (0, -7, -10), Plane.normal = (0, 1, 0) }
         ]
-}
o = (0, 0, -10)
angle = pi / 6

rotateZY :: Double -> Vector -> Vector
rotateZY theta (x,y,z) = (x, c * z - s * y, s * z + c * y)
   where c = cos theta
         s = sin theta

