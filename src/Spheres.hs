module Spheres where
import Rays
import Tuples
import Matrices
import qualified Data.List as List
import Transformations
import Materials
import State
data Sphere = Sphere {getId :: Int, 
                      getPos :: Tuple Double, 
                      getR :: Double, 
                      getTransform :: Matrix Double,
                      getMaterial :: Material}
 deriving Show

data Intersection = Intersection {time :: Time, object :: Sphere}
 deriving Show
instance Eq Intersection where
    (==) a b = time a == time b && (getId . object) a == (getId . object) b
-- We are looking for the lowest non-negative value of t (time)
instance Ord Intersection where
    (<=) a b = time a <= time b && time a >= 0

sphere :: (Int, Tuple Double, Double, Matrix Double, Material) -> Sphere
sphere (id, pos, r, t, m) = Sphere id pos r t m 

defaultSphere :: Int -> Sphere
defaultSphere id = sphere (id, (point 0 0 0), 1, identityMatrix, defaultMaterial)

setTransform' :: Sphere -> (a -> Matrix Double) -> a -> Sphere
setTransform' s f a = sphere (getId s, getPos s, getR s, f a, getMaterial s )

setTransform :: Sphere -> Matrix Double -> Sphere
setTransform s a = sphere (getId s, getPos s, getR s, a, getMaterial s )

hit :: State [Intersection] (Maybe Intersection)
hit = do
    xs <- ask
    return (headOr ((List.sort . filter (\x -> time x >= 0)) xs))
    --  

headOr :: [a] ->  Maybe a
headOr [] = Nothing
headOr (x:_) = Just x

{-
There are 4 vectors calculated after an intersection:
* (E) the eye vector (negate the ray's direction)
* (L) the light vector (subtract P fromo the position of the light source)
* (N) the normal vector (perpendicular to the surface hit)
* (R) the reflection vector, pointing in the directioin that incoming light would go.
-}

normalsAt :: (Sphere, Tuple Double) -> Tuple Double
normalsAt (s,p) = vector (getX wordNormal) (getY wordNormal) (getZ wordNormal)
    where objectPoint = matrixVectorMultiply (inverse (getTransform s)) p
          objectNormal = objectPoint - (getPos s)
          wordNormal = matrixVectorMultiply (transpose (inverse (getTransform s))) objectNormal



it0 = intersection (3.5, s)
it1 = intersection (2.5, s)
it2 = intersection (1.5, s)
it3 = intersection (0.5, s)
it4 = intersection (-1.5, s)
it5 = intersection (-2.5, s)
it6 = intersection (-3.5, s)

intersection :: (Time, Sphere) -> Intersection
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

{-
usage of intersect:

example:

eval (intersect (s, r1)) []
[Intersection {time = 5.0, object = Sphere {getId = 1, getPos = Tuple {getX = 0.0, getY = 0.0, getZ = 0.0, getW = 1.0}, getR = 1.0}},Intersection {time = 5.0, object = Sphere {getId = 1, getPos = Tuple {getX = 0.0, getY = 0.0, getZ = 0.0, getW = 1.0}, getR = 1.0}}]
-}

intersect :: (Sphere, Ray) -> State [Intersection] (Maybe [Intersection])
intersect (s,r') = let d = (discriminant a b c) 
                  in if d < 0 then return Nothing
                              else return (Just (quadraticEquation d))
 where r = transform ((inverse . getTransform) s) r'
       sphereToRay = origin r - (getPos s)
       a = dot (direction r) (direction r)
       b = 2 * dot (direction r) sphereToRay
       c = dot sphereToRay sphereToRay - 1
       quadraticEquation d = map (\x -> intersection (x / (2 * a), s)) ((negate b) ± (sqrt d)) 

r1 = ray ((point 0 0 (-5)), vector 0 0 1)

s = defaultSphere 1

test = eval (intersect (s1, r1)) []
    where s1 = setTransform' s scalingMatrix (2.0, 2.0,2.0) --TODO: FIX this should be (2.0, 2.0, 2.0)


-- xs = intersect (s, r1)

infixl 6 ±

(±) :: Num a => a -> a -> [a]
-- ascending order, recommended by book
(±) a b = [a - b, a + b]