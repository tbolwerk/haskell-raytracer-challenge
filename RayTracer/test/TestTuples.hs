import Test.QuickCheck
import ../feature/Tuple
--TODO: [ ] Create test with QuickCheck
--- A tuple with w = 1.0 is a point
--- A tuple with w = 0.0 is a vector
testPoint = Tuple 4.3 (-4.2) 3.1 1.0
testVector = Tuple 4.3 (-4.2) 3.1 0.0 

testCases = [testPoint, testVector]

test = (map getTuple $ map getPrimitive testCases) == testCases


-- Adding two tuples
a1 = Tuple
