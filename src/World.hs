{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module World where

import           Colors
import           Control.Monad
import qualified Data.List       as List
import           Hitable
import           Lights
import           LinearAlgebra
import           Materials
import           Rays
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
intersectWorld (w,r) = hit $ eval (foldM (\acc o -> do
    xs <- intersect (o,r)
    return (xs <> acc) ) [] (objects w)) []





world :: [Object] -> [Light] -> World
world = World

defaultWorld :: World
defaultWorld =
  World
    { objects =
        [
         (Object (sphere
             ( 2
             , point (0, 0, 0)
             , 1.0
             , identityMatrix
             , defaultMaterial
                 { Materials.color = (Colors.color 0.8 1.0 0.6 0)
                 , ambient = 1
                 , diffuse = 0.7
                 , specular = 0.2
                 , shininess = 200.0
                 })))
        , (Object (setTransform (defaultSphere 1) (scalingMatrix (0.5, 0.5, 0.5))))
        ]
    , lights = [
                (pointLight (point (-10, 10, -10), Colors.color 1 1 1 0))
            --    ,(pointLight (point (10, -10, 10), Colors.color 1 1 1 0))
               ]
    }


shadeHit :: (World, Computation) -> Colors.Color
shadeHit (w, comp) = foldr (\l r -> let s = isShadowed l (w,p) in lighting (m,o,l,p,e,n,s) + r) (Colors.color 0 0 0 0) (lights w)
 where m = (Hitable.getMaterial . computationObject) comp
       p = computationOverPoint comp
       e = computationEye comp
       n = computationNormal comp
       o = computationObject comp

colorAt :: (World, Ray) -> Colors.Color
colorAt (w,r) = case (intersectWorld (w,r)) of
                   [] -> Colors.color 0 0 0 1
                   (x:_) -> let comp = prepareComputation (x, r)
                            in  shadeHit (w,comp)



isShadowed :: Light -> (World, Tuple Double) -> Bool
{-# INLINE isShadowed #-}
isShadowed l (w, p) = let v = Lights.position l
                          dists =  magnitude v
                          dirs =  normalize v
                          rs = ray (p, dirs)
                          is = intersectWorld (w,rs)
                          h = hit is
                     in case h of
                          (x:_) -> time x < dists
                          [] -> False