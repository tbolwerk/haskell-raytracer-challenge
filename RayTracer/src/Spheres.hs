module Spheres where
import Rays
import Tuples

data Sphere = Sphere {getId :: Int, getPos :: Tuple Double, getR :: Double}
 deriving Show

data Intersection = Intersection {time :: Time, object :: Sphere}
 deriving Show

sphere :: Int -> Sphere
sphere id = Sphere id (point 0 0 0) 1

it = intersection (3.5, s)

intersection :: (Time, Sphere) -> Intersection
intersection (t, s) = Intersection t s

intersections :: Intersection -> State [Intersection] [Intersection]
intersections is = do 
    put [is]
    get 

exec :: State s a -> s -> s
exec m s = snd (getState m s)

eval :: State s a -> s -> a
eval m s = fst (getState m s)

{-
Use state monad too keep track of all intersections!

example usage main function:
-}
main :: State [Intersection] [Intersection]
main = do
    put [it]
    put [it]
    get
        
get :: State a a 
get =  State $ \s -> (s,s)
put :: Monoid s => s -> State s ()
put s = State $ \s0 -> ((), s <> s0)


newtype State s a = State { getState :: s -> (a, s)}

instance Functor (State s) where
    fmap f fa = State $ \s0 -> let (a,s1) = (getState fa) s0
                                in (f a, s1)
instance Applicative (State s) where
    pure a = State $ \s -> (a,s)
    (State af) <*> (State aa) = State $ \s0 -> let (f, s1) = af s0
                                               in let (a,s2) = aa s1
                                                   in (f a, s2)
instance Monad (State a) where
    return = pure
    ma >>= f = State $ \s0  -> let (a ,s1) = getState ma s0 
                                in getState (f a) s1

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

intersect :: (Sphere, Ray) -> State [Intersection] [Intersection]
intersect (s,r) = let d = (discriminant a b c) 
                  in if d < 0 then return []
                              else return (quadraticEquation d)
 where sphereToRay = origin r - (getPos s)
       a = dot (direction r) (direction r)
       b = 2 * dot (direction r) sphereToRay
       c = dot sphereToRay sphereToRay - 1
       quadraticEquation d = map (\x -> intersection (x / (2 * a), s)) ((negate b) ± (sqrt d)) 

r1 = ray ((point 0 1 (-5)), vector 0 0 1)

s = sphere 1

-- xs = intersect (s, r1)

infixl 6 ±

(±) :: Num a => a -> a -> [a]
-- ascending order, recommended by book
(±) a b = [a - b, a + b]