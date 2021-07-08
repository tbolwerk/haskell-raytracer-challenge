module Spheres where
import Rays
import Tuples

data Sphere = Sphere {getId :: Int, getPos :: Tuple Double, getR :: Double}
 deriving Show

sphere :: Int -> Sphere
sphere id = Sphere id (point 0 0 0) 1


{-
abc formula
x = (-b ±sqrt(b^2 - 4*a*c))/(2 * a)
discriminant = b^2 - 4*a*c)
-}
discriminant :: Double -> Double -> Double -> Double
discriminant a b c = b^2 - 4 * a * c

intersect :: (Sphere, Ray) -> [Double]
intersect (s,r) = let d = (discriminant a b c) 
                  in if d < 0 then []
                              else (quadraticEquation d)
 where sphereToRay = origin r - (getPos s)
       b = 2 * dot (direction r) sphereToRay
       a = dot (direction r) (direction r)
       c = dot sphereToRay sphereToRay - 1
       quadraticEquation d = map (\x -> x / (2 * a)) ((negate b) ± (sqrt d)) 

r1 = ray ((point 0 0 (-5)), vector 0 0 1)

s = sphere 1

-- xs = intersect (s, r1)

infixl 6 ±

(±) :: Num a => a -> a -> [a]
(±) a b = [a - b, a + b]