{-# LANGUAGE Strict #-}
module Chapter7 where
import Spheres
import Materials
import Colors
import Transformations
import LinearAlgebra
import State 
import Canvas
import Data.Array
import World
import Lights
import Hitable
import Camera
import           Control.Concurrent.Async

canvasPixels = 1000

floor' = sphere (1,point (0,0,0) ,1.0 ,scalingMatrix (10,0.01,10),m)
    where m = material (Colors.color 1 0.9 0.9 1, 0.1, 0.9, 0.0, 200.0)

leftWall = sphere (2,point (0,0,0) ,1.0 , leftWallTransformation ,m)
    where m = material (Colors.color 1 0.9 0.9 1, 0.1, 0.9, 0.0, 200.0)

rightWall = sphere (3,point (0,0,0) ,1.0 , rightWallTransformation ,m)
    where m = material (Colors.color 1 0.9 0.9 1, 0.1, 0.9, 0.0, 200.0)

middle = sphere (4, point (0,0,0), 1.0, translationMatrix (negate 0.5, 1,0.5), m)
 where m = defaultMaterial { Materials.color = Colors.color 0.1 1 0.5 1
  , diffuse = 0.7
  , specular = 0.3
  }

right = sphere (5,point (0,0,0), 1.0, translationMatrix (1.5,0.5,negate 0.5) * scalingMatrix (0.5,0.5,0.5),m)
 where m = defaultMaterial { Materials.color = Colors.color 0.5 1 0.1 1
  , diffuse = 0.7
  , specular = 0.3
  }

left = sphere (6, point (0,0,0), 1.0, translationMatrix (negate 1.5, 0.33,negate 0.75) * scalingMatrix (0.33, 0.33, 0.33),m)
 where m = defaultMaterial { Materials.color = Colors.color 1 0.8 0.1 1
  , diffuse = 0.7
  , specular = 0.3
  }

leftWallT = translation (0,0,5) . rotateY (negate (pi/4)) . rotateX (pi/2) . scaling (10,0.01,10)
leftWallTransformation = f $ leftWallT (tuple (1,1,1,1))
-- leftWallTransformation = translationMatrix (0,0,5) * rotateYMatrix (((negate pi)/4)) * rotateXMatrix (pi/2) * scalingMatrix (10,0.01,10)
rightWallTransformation = translationMatrix (0,0,5) * rotateYMatrix ((pi/4)) * rotateXMatrix (pi/2) * scalingMatrix (10,0.01,10)


f (Tuple x y z w) = listToMatrix 4 4 (concat [[x, 0 ,0 ,0],[0,y,0,0],[0,0,z,0],[0,0,0,w]])

world1 = world [(Object floor'), (Object leftWall), (Object rightWall), (Object middle), (Object left), (Object right)] [(pointLight (point (-10, 10, -10), Colors.color 1 1 1 0))]
world2 = world [(Object floor'), (Object leftWall), (Object rightWall), (Object middle), (Object left), (Object right)] [(pointLight (point (10, 10, 10), Colors.color 0.3 0.3 0.3 0)), (pointLight (point (10, 10, -10), Colors.color 1 1 1 0))]
-- world' = defaultWorld
cam = setViewTransform (camera (canvasPixels,canvasPixels `div` 2,pi/3)) (viewTransformation (point (0,1.5,negate 5), point (0,1,0), vector (0,1,0)))

main = mapConcurrently execute [(world1, "chapter7.ppm"),(world2, "chapter7_1.ppm")]
    
execute (w,n) = createPPM (Canvas (hSize cam -1 ) (vSize cam-1) (listArray ((0,0),(hSize cam-1,vSize cam-1)) (eval (render (cam, w)) []))) n