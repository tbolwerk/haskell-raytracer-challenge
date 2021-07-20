{-# LANGUAGE ExistentialQuantification #-}
module Shape where
import           LinearAlgebra
import           Materials
import           Rays
import           State


data Intersection = Intersection {
                                   time   :: !Time
                                 , object :: !Shape
                                 }
 deriving Show
instance Eq Intersection where
    (==) a b = time a == time b && (identifier . object) a == (identifier . object) b
-- We are looking for the lowest non-negative value of t (time)
instance Ord Intersection where
    (<=) a b = time a <= time b && time a >= 0

class Shape_ a where
   identifier :: a -> Int
   position       :: a -> Tuple Double
   perimeter         :: a -> Double
   transformation :: a -> Matrix Double
   material  :: a -> Material
   intersect :: (a, Ray) -> State [Intersection] [Intersection]

data Shape = forall a. Shape_ a =>  Shape a
instance Show Shape where
    show a = show (identifier a)

instance Shape_ Shape where
  identifier (Shape a) = identifier a
  position (Shape a) = Shape.position a
  perimeter (Shape a) = perimeter a
  transformation (Shape a) = transformation a
  material (Shape a) = Shape.material a
  intersect (Shape a, r) = Shape.intersect (a,r)
