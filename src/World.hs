module World where

import           Colors
import           Control.Monad
import qualified Data.List       as List
import           Lights
import           LinearAlgebra
import           Materials
import           Rays
import           Hitable
import           Spheres
import           State
import           Transformations
data World =
  World
    { objects :: [Object]
    , lights  :: [Light]
    }
 deriving Show


intersectWorld' :: (World, Ray) -> [Intersection]
intersectWorld' (w, r) = List.sort $ (foldr (\s xs -> (eval (intersect (s,r)) []) <> xs) [] (objects w))

intersectWorld :: (World, Ray) -> [Intersection]
intersectWorld (w,r) = List.sort $ eval (foldM (\acc o -> do
    xs <- intersect (o,r)
    return (xs <> acc) ) [] (objects w)) []





world :: [Object] -> [Light] -> World
world = World

defaultWorld :: World
defaultWorld =
  World
    { objects =
        [ (Object (setTransform (defaultSphere 1) (scalingMatrix (0.5, 0.5, 0.5))))
        , (Object (sphere
             ( 2
             , point (0, 0, 0)
             , 1.0
             , identityMatrix
             , Material
                 { Materials.color = (Colors.color 0.8 1.0 0.6 1)
                 , ambient = 0.7
                 , diffuse = 0
                 , specular = 0.2
                 , shininess = 0.0
                 })))
        ]
    , lights = [(pointLight (point (-10, 10, -10), Colors.color 1 1 1 1))]
    }
