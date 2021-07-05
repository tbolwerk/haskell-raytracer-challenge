{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANUGAGE DatatypeContexts #-}
import Control.Monad

epsilon = 0.00001

isPoint :: (Fractional a, Num a, Eq a) => Tuple a -> Bool
isPoint tuple = getW tuple == 1.0

point :: (Fractional a, Num a) => a -> a -> a -> Tuple a 
point x y z = Tuple x y z 1.0

isVector :: (Num a,Fractional a, Eq a) => Tuple a -> Bool
isVector tuple = getW tuple == 0.0

vector :: (Fractional a, Num a) => a -> a -> a -> Tuple a
vector x y z = Tuple x y z 0.0

tuple :: Num a =>  a -> a -> a -> a -> Tuple a
tuple = Tuple

magnitude :: (Floating a, Num a) => Tuple a -> a
magnitude (Tuple x y z w) = sqrt ((x^2) + (y^2) + (z^2) + (w^2))

normalize :: (Floating a, Num a) => Tuple a -> Tuple a
normalize t@(Tuple x y z w) = tuple (x / magnitude t) (y / magnitude t) (z / magnitude t) (w / magnitude t) 

dot :: (Floating a, Num a) => Tuple a -> Tuple a -> a
dot (Tuple x1 y1 z1 w1) (Tuple x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)  

cross :: (Eq a,Floating a, Num a) => Tuple a -> Tuple a -> Tuple a
cross (Tuple x1 y1 z1 0) (Tuple x2 y2 z2 0) = vector ((y1 * z2) - (z1 * y2))  ((x1 * z2) - (z1 * x2))  ((x1 * y2) - (x2 * y1))

data Projectile a = Projectile { position :: Tuple a, velocity :: Tuple a} 
 deriving Show

data Environment a = Environment { gravity :: Tuple a, wind :: Tuple a }
--- s = Projectile
--- a = Enviroment

runScenario :: (Environment Double) -> Scenario (Projectile Double) (Projectile Double)
runScenario env = (Scenario $ \s -> (tick2 env s, tick2 env s)) 

scene = do 
 let s1 = fst $ tick (runScenario e) p
     s2 = fst $ tick (runScenario e) s1
     s3 = fst $ tick (runScenario e) s2
 tick (runScenario e) s3

newtype Scenario s a = Scenario { tick :: s -> (a, s) } 

instance Functor (Scenario s) where
 fmap f x = Scenario $ \s0 ->  let (a, s1) = (tick x s0)
                               in (f a, s1)

instance Applicative (Scenario s) where
 pure a = Scenario $ \s -> (a, s)
 sf <*> sa = Scenario $ \s0 -> let (fs, s1) = (tick sf s0)
                                   (a, s2) = (tick sa s1)
                               in (fs a, s2) 


instance Monad (Scenario s) where
 return a = pure a
 sa >>= asb = Scenario $ \s0 -> let (a, s1) = tick sa s0
                                in (tick (asb a) s1)





tick2 :: (Fractional a) =>  Environment a -> Projectile a -> Projectile a
tick2 env proj = let pos = position proj + velocity proj 
                in let vel =  velocity proj + gravity env + wind env
                   in Projectile pos vel 

p = Projectile (point 0.0 1.0 0.0) (normalize (vector 1.0 1.0 0.0))
e = Environment (vector 0.0 (-0.1) 0.0) (vector (-0.001) 0 0)

data Tuple a = Tuple { getX :: a, getY :: a, getZ :: a, getW :: a }
 deriving (Show)

instance Eq (Tuple Double) where
 Tuple x1 y1 z1 w1 == Tuple x2 y2 z2 w2 = (compare x1 x2) && (compare y1 y2) && (compare z1 z2) && (compare w1 w2) 
  where compare a b = abs (a - b) < epsilon

instance Functor Tuple where
 fmap f (Tuple x y z w) = Tuple (f x) (f y) (f z) (f w) 

instance Applicative Tuple where
 pure x = Tuple x x x x
 (Tuple x1 y1 z1 w1) <*> (Tuple x2 y2 z2 w2) = Tuple (x1 x2) (y1 y2) (z1 z2) (w1 w2)

instance (Num a, Fractional a) => Num (Tuple a) where
 a + b = (fmap (+) a) <*> b
 a - b = (fmap (-) a) <*> b
 negate a = vector 0 0 0 - a
 a * b = (fmap (*) a) <*> b

instance (Fractional a) => Fractional (Tuple a) where
 a / b = (fmap (/) a) <*> b
 


a1 = Tuple 3 (-2) 5 1
a2 = Tuple (-2) 3  1 0

p1 = point 3.0 2.0 1.0
p2 = point 5.0 6.0 7.0 

v1 = vector 3 2 1
v2 = vector 5 6 7

a = tuple 1.0 (-2.0) 3.0 (-4.0)



