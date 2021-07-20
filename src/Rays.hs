
module Rays where

{-
Literaly the example

https://wiki.haskell.org/Existential_type

-}

import           LinearAlgebra
import           Transformations

type Vector = Tuple Double
type Point = Tuple Double
type Time = Double

data Ray = Ray {
                 origin    :: !Point
               , direction :: !Vector}
 deriving Show





ray :: (Point, Vector) -> Ray
ray (p, v) = Ray p v

position :: (Ray, Time) -> Point
position (r, t) = origin r + direction r * pure t

transform :: Matrix Double -> Ray -> Ray
transform m r = ray ((matrixVectorMultiply m (origin r)), (matrixVectorMultiply m (direction r)))

{-
use as:
transform' scaling (2,3,4) r
-}
transform' :: (a -> Tuple Scalar -> Tuple Scalar) -> a -> Ray -> Ray
transform' f a r = ray ((f a (origin r)), (f a (direction r)))

r = ray ((point (1, 2, 3)), (vector (0, 1, 0)))
m = scaling (2,3,4)
