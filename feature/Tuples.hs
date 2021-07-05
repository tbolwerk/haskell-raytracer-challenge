import Test.QuickCheck
--TODO: [ ] Create test with QuickCheck
--- A tuple with w = 1.0 is a point
--- A tuple with w = 0.0 is a vector
testPoint = Tuple 4.3 (-4.2) 3.1 1.0
testVector = Tuple 4.3 (-4.2) 3.1 0.0 

testCases = [testPoint, testVector]

test = (map getTuple $ map getPrimitive testCases) == testCases


--- Start Source code
epsilon :: Double
epsilon = 0.00001

data Tuple = Tuple { getX :: Double, getY :: Double, getZ :: Double, getW :: Double }
 deriving (Show)

instance Eq Tuple where
 Tuple x1 y1 z1 w1 == Tuple x2 y2 z2 w2 = (compare x1 x2) && (compare y1 y2) && (compare z1 z2) && (compare w1 w2) 
  where compare a b = abs (a - b) < epsilon

data Primitive = Point Double Double Double | Vector Double Double Double
 deriving (Show)
instance Eq Primitive where
 a == b = getTuple a == getTuple b

getTuple :: Primitive -> Tuple
getTuple (Point x y z) = Tuple x y z 1.0
getTuple (Vector x y z) = Tuple x y z 0.0

getPrimitive :: Tuple -> Primitive   
getPrimitive (Tuple x y z 1.0) = Point x y z
getPrimitive (Tuple x y z 0.0) = Vector x y z
