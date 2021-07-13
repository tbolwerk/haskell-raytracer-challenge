{-# LANGUAGE FlexibleInstances #-}
module Tuples where
{- 
This module is part of chapter 1 of The Ray Tracer Challenge

This is the library for tuples, one of the backbones of a Raytracer. Used in all kinds of calculations.

For example when light hits an object. 

Some keynotions are
  vector = tuple (x y z 0) 
   w = 0
  point = tuple (x y z 1)
   w = 1

  vector is used for motions and forcer, hence the w = 0
  point is visable and rather small object, therefore the w = 1
-}   
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
cross (Tuple x1 y1 z1 0) (Tuple x2 y2 z2 0) = vector ((y1 * z2) - (z1 * y2)) ((z1 * x2) - (x1 * z2)) ((x1 * y2) - (y1 * x2)) 

reflect :: (Tuple Double, Tuple Double) -> Tuple Double
reflect (v,n) = v - n * pure (2 * dot v n)

data Tuple a = Tuple { 
                       getX :: !a
                     , getY :: !a
                     , getZ :: !a
                     , getW :: !a 
                     }
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