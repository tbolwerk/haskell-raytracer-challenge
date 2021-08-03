{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}


module Spheres where
import qualified Data.List       as List
import           LinearAlgebra
import           Materials
import           Rays
import           State
import           Transformations

data Sphere = Sphere {getId        :: !Int,
                      getPos       :: !(Tuple Double),
                      getR         :: !Double,
                      getTransform :: !(Matrix Double),
                      getMaterial  :: !Material}
 deriving Show

sphere :: (Int, Tuple Double, Double, Matrix Double, Material) -> Sphere
sphere (id, pos, r, t, m) = Sphere id pos r t m

defaultSphere :: Int -> Sphere
defaultSphere id = sphere (id, (point (0, 0, 0)), 1, identityMatrix, defaultMaterial)

setTransform' :: Sphere -> (a -> Matrix Double) -> a -> Sphere
setTransform' s f a = sphere (getId s, getPos s, getR s, f a, getMaterial s )

setTransform :: Sphere -> Matrix Double -> Sphere
setTransform s a = sphere (getId s, getPos s, getR s, a, getMaterial s)

setMaterial :: Sphere -> Material -> Sphere
setMaterial s m = sphere (getId s, getPos s, getR s, getTransform s, m)
{-
There are 4 vectors calculated after an intersection:
* (E) the eye vector (negate the ray's direction)
* (L) the light vector (subtract P fromo the position of the light source)
* (N) the normal vector (perpendicular to the surface hit)
* (R) the reflection vector, pointing in the directioin that incoming light would go.
-}



-- it0 = intersection (3.5, s)
-- it1 = intersection (2.5, s)
-- it2 = intersection (1.5, s)
-- it3 = intersection (0.5, s)
-- it4 = intersection (-1.5, s)
-- it5 = intersection (-2.5, s)
-- it6 = intersection (-3.5, s)


-- intersections :: Intersection -> State [Intersection] [Intersection]
-- intersections is = do
--     put [is]
--     ask


{-
Use state monad too keep track of all intersections!

example usage main function:
-}
-- main :: State [Intersection] [Intersection]
-- main = do
--     put [it0]
--     -- put [it1]
--     -- put [it2]
--     -- put [it3]
--     put [it4]
--     put [it5]
--     put [it6]
--     ask


{-
abc formula
x = (-b ±sqrt(b^2 - 4*a*c))/(2 * a)
discriminant = b^2 - 4*a*c)
-}

{-
usage of intersect:

example:

eval (intersect (s, r1)) []
[Intersection {time = 5.0, object = Sphere {getId = 1, getPos = Tuple {getX = 0.0, getY = 0.0, getZ = 0.0, getW = 1.0}, getR = 1.0}},Intersection {time = 5.0, object = Sphere {getId = 1, getPos = Tuple {getX = 0.0, getY = 0.0, getZ = 0.0, getW = 1.0}, getR = 1.0}}]
-}




-- r1 = ray ((point (0, 0, (-5))), vector (0, 0, 1))

-- s = defaultSphere 1

-- test = eval (intersect (s1, r1)) []
--     where s1 = setTransform' s scalingMatrix (2.0, 2.0,2.0) --TODO: FIX this should be (2.0, 2.0, 2.0)


-- xs = intersect (s, r1)


