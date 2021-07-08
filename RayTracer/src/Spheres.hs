module Spheres where
import Rays
import Tuples

data Sphere = Sphere {getId :: Int, getPos :: Tuple Double, getR :: Double}
 deriving Show

sphere :: Int -> Sphere
sphere id = Sphere id (point 0 0 0) 1


-- intersect :: (Sphere, Ray) -> [Double]
intersect (s,r) = [position (r, t) | t <- [0..10], (position (r, t) == sphereBounds)]
    where sphereBounds = (getPos s) 
            --   + (vector (getR s) (getR s) (getR s))

r1 = ray ((point 0 0 (-5)), vector 0 0 1)

s = sphere 1

-- xs = intersect (s, r1)