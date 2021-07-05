{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANUGAGE DatatypeContexts #-}
epsilon = 0.00001

newtype Scalar a = Scalar { getScalar :: Double } 

isPoint :: (Fractional a, Num a, Eq a) => Tuple a -> Bool
isPoint tuple = getW tuple == 1

point :: (Fractional a, Num a) => a -> a -> a -> Tuple a 
point x y z = Tuple x y z 1

isVector :: (Num a,Fractional a, Eq a) => Tuple a -> Bool
isVector tuple = getW tuple == 0

vector :: Num a => a -> a -> a -> Tuple a
vector x y z = Tuple x y z 0

tuple :: Num a =>  a -> a -> a -> a -> Tuple a
tuple = Tuple

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

instance (Num a) => Num (Tuple a) where
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

p = point 3 2 1
v = vector 5 6 7

v1 = vector 3 2 1
v2 = vector 5 6 7

a = tuple 1.0 (-2.0) 3.0 (-4.0)


f :: Monoid a =>a ->  (a,a) 
f = pure 

