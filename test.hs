import Scene
import Render
import Shapes
import Intersection

scene = [MkSceneObject $ Sphere { center = (0, 0, -5), radius = 0.01 }]

main = putStr . show $ render (10, 10) scene

