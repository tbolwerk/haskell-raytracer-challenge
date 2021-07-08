module Rays where

import Tuples
import Matrices

type Vector = Tuple Double
type Point = Tuple Double
type Time = Double
data Ray = Ray {origin :: Point, direction :: Vector}
 deriving Show

ray :: (Point, Vector) -> Ray
ray (p, v) = Ray p v

position :: (Ray, Time) -> Point
position (r, t) = origin r + direction r * pure t

r = ray ((point 2 3 4), (vector 1 0 0))