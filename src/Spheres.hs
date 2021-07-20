{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict       #-}
{-# LANGUAGE StrictData   #-}
module Spheres where
import qualified Data.List       as List
import           LinearAlgebra
import           Materials
import           Rays
import           Shape
import           State
import           Transformations
data Sphere = Sphere {getId        :: !Int,
                      getPos       :: !(Tuple Double),
                      getR         :: !Double,
                      getTransform :: !(Matrix Double),
                      getMaterial  :: !Material}
 deriving (Show)

instance Shape_ Sphere where
    identifier = getId
    position = getPos
    perimeter = getR
    transformation = getTransform
    material = getMaterial
    intersect :: (Sphere, Ray) -> State [Intersection] [Intersection]
    {-# INLINE intersect #-}
    intersect (s,r') = let d = (discriminant a b c)
                  in if d < 0 then return []
                              else return (hit (quadraticEquation d))
      where r :: Ray
            r = transform ((inverse . Shape.transformation) s) r'
            sphereToRay :: Tuple Double
            sphereToRay = origin r - (Shape.position s)
            a :: Double
            a = dot (direction r) (direction r)
            b :: Double
            b = 2 * dot (direction r) sphereToRay
            c :: Double
            c = dot sphereToRay sphereToRay - 1
            quadraticEquation :: Double -> [Intersection]
            quadraticEquation d = map (\x -> intersection (x / (2 * a), (Shape s))) ((negate b) ± (sqrt d))


sphere :: (Int, Tuple Double, Double, Matrix Double, Material) -> Shape
sphere (id, pos, r, t, m) = (Shape (Sphere id pos r t m))

defaultSphere :: Int -> Shape
defaultSphere id = sphere (id, (point (0, 0, 0)), 1, identityMatrix, defaultMaterial)

setTransform' :: Shape -> (a -> Matrix Double) -> a -> Shape
setTransform' s f a = sphere ((Shape.identifier) s, (Shape.position) s, (Shape.perimeter) s, f a, (Shape.material) s )

setTransform :: Shape -> Matrix Double -> Shape
setTransform s a = sphere ((Shape.identifier) s, (Shape.position) s, (Shape.perimeter) s, a, (Shape.material) s )


hit :: [Intersection] -> [Intersection]
hit xs= ((List.sort . filter (\x -> time x >= 0)) xs)
{-# INLINE hit #-}

{-
There are 4 vectors calculated after an intersection:
* (E) the eye vector (negate the ray's direction)
* (L) the light vector (subtract P fromo the position of the light source)
* (N) the normal vector (perpendicular to the surface hit)
* (R) the reflection vector, pointing in the directioin that incoming light would go.
-}

normalsAt :: (Shape, Tuple Double) -> Tuple Double
normalsAt (s,p) = normalize worldNormal
    where objectPoint = matrixVectorMultiply (inverse (Shape.transformation s)) p
          objectNormal = objectPoint - (Shape.position) s --TODO: World point
          Tuple x y z _ = matrixVectorMultiply (transpose (inverse (Shape.transformation s))) objectNormal
          worldNormal = vector (x, y, z)
{-# INLINE normalsAt #-}


it0 = intersection (3.5, s)
it1 = intersection (2.5, s)
it2 = intersection (1.5, s)
it3 = intersection (0.5, s)
it4 = intersection (-1.5, s)
it5 = intersection (-2.5, s)
it6 = intersection (-3.5, s)

intersection :: (Time, Shape) -> Intersection
intersection (t, s) = Intersection t s

intersections :: Intersection -> State [Intersection] [Intersection]
intersections is = do
    put [is]
    ask


{-
Use state monad too keep track of all intersections!

example usage main function:
-}
main :: State [Intersection] [Intersection]
main = do
    put [it0]
    -- put [it1]
    -- put [it2]
    -- put [it3]
    put [it4]
    put [it5]
    put [it6]
    ask


{-
abc formula
x = (-b ±sqrt(b^2 - 4*a*c))/(2 * a)
discriminant = b^2 - 4*a*c)
-}
discriminant :: Double -> Double -> Double -> Double
discriminant a b c = b^2 - 4 * a * c
{-# INLINE discriminant #-}
{-
usage of intersect:

example:

eval (intersect (s, r1)) []
[Intersection {time = 5.0, object = Shape {getId = 1, getPos = Tuple {getX = 0.0, getY = 0.0, getZ = 0.0, getW = 1.0}, getR = 1.0}},Intersection {time = 5.0, object = Shape {getId = 1, getPos = Tuple {getX = 0.0, getY = 0.0, getZ = 0.0, getW = 1.0}, getR = 1.0}}]
-}



r1 = ray ((point (0, 0, (-5))), vector (0, 0, 1))

s = defaultSphere 1

test = eval (intersect (s1, r1)) []
    where s1 = setTransform' s scalingMatrix (2.0, 2.0,2.0) --TODO: FIX this should be (2.0, 2.0, 2.0)


-- xs = intersect (s, r1)

infixl 6 ±

(±) :: Num a => a -> a -> [a]
-- ascending order, recommended by book
(±) a b = [a - b, a + b]
