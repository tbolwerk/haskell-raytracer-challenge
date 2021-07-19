module TuplesTest where
import           Test.HUnit
import qualified Test.QuickCheck as T
import           Tuples
type TupleInput = (Double,Double,Double,Double)

tupleIsPoint :: Tuple Double
tupleIsPoint = (tuple (4.3 ,(-4.2), 3.1 ,1.0))
tupleIsVector :: Tuple Double
tupleIsVector = (tuple (4.3 ,(-4.2), 3.1, 0.0))
vectorIsTuple :: Tuple Double
vectorIsTuple = (vector (4.3, (-4.2), 3.1))
pointIsTuple :: Tuple Double
pointIsTuple = point (4.3 ,(-4.2), 3.1)

testCase1 = assertBool "is a point," (isPoint tupleIsPoint)
testCase2 = assertBool "is a vector," (isVector tupleIsVector)
testCase3 = assertEqual "tuple with w=0.0"  vectorIsTuple tupleIsVector
testCase4 = assertEqual "tuple with w=1.0" pointIsTuple tupleIsPoint
testCase5 = assertEqual "tuple (1, 1, 6, 1)" (tuple (1,1,6,1)) (tuple (3 ,(-2 :: Double) ,5 ,1) + tuple (-2,3,1,0))
testCase6 = assertEqual "vector (-2, -4, -6)" (vector ((-2) ,(-4 :: Double), (-6))) (point (3, 2, 1) - point (5, 6, 7))
testCase7 = assertEqual "point (-2, -4, -6)" (point ((-2), (-4) ,(-6::Double))) (point ((3::Double) ,2, 1) - vector (5 ,6 ,7))
testCase8 = assertEqual "vector (-2, -4, -6)" (vector ((-2) ,(-4), (-6 :: Double))) ((vector (3, 2, 1)) - (vector (5, 6, (7 :: Double))))
testCase9 = assertEqual "vector (-1, 2, -3)" (vector ((-1) ,2 ,(-3 :: Double))) ((vector (0,0,0)) - (vector (1, (-2 :: Double) ,3)))
testCase10 = assertEqual "vector (-1, 2, -3)" (tuple ((-1), 2 ,((-3) :: Double), 4))  (negate (tuple (1, (-2) ,3, (-4))))
testCase11 = assertEqual "a * 3.5" (tuple (3.5, (-7 ::Double) ,10.5, (-14))) ((tuple (1 ,(-2) ,3 ,(-4))) * pure 3.5)
testCase12 = assertEqual "a * 0.5" (tuple (0.5 ,(-1 ::Double) ,1.5, (-2))) ((tuple (1 ,(-2) ,3 ,(-4))) * pure 0.5)
testCase13 = assertEqual "a / 2" (tuple (0.5, (-1) ,1.5, (-2 ::Double))) ((tuple (1, (-2), 3, (-4))) / pure 2)
testCase14 = assertEqual "magnitude(vector (1,0,0))" 1 (magnitude (vector (1,0,0)))
testCase15 = assertEqual "magnitude(vector (0,1,0))" 1 (magnitude (vector (0, 1, 0)))
testCase16 = assertEqual "magnitude(vector (0,0,1))" 1 (magnitude (vector (0,0, 1)))
testCase17 = assertEqual "magnitude(vector (1,2,3))" (sqrt 14) (magnitude (vector (1,2,3)))
testCase18 = assertEqual "magnitude(vector (-1,-2,-3))" (sqrt 14) (magnitude (vector ((-1), (-2), (-3))))
testCase19 = assertEqual "normalize(vector (1,0,0)" (vector ((1 :: Double), 0, 0)) (normalize (vector (4,0,0)))
testCase20 = assertEqual "normalize(vector (1,2,3)" (vector ((1/(sqrt 14)) ,(2/(sqrt (14 :: Double))), (3/sqrt 14))) (normalize (vector (1,2,3)))
testCase21 = assertEqual "magnitude(normalize(vector (1,2,3))" 1 (magnitude (normalize (vector (1,2,3))))
testCase22 = assertEqual "dot(a,b) = 20" 20 (dot (vector (1,2,3)) (vector (2,3,4)))
testCase23 = assertEqual "cross(a,b) = vector(-1,2,-1)" (vector ((-1), 2, (-1 ::Double))) (cross (vector (1,2,3)) (vector (2,3,4)))
testCase24 = assertEqual "cross(b,a) = vector(1,-2,1)" (vector (1, (-2 ::Double), 1)) (cross (vector (2,3,4)) (vector (1,2,3)))

{-
Scenario: A tuple with w = 1.0 is a point
    Given a <- tuple (4.3, -4.2, 3.1, 1.0)
    Then a.x = 4.3
         a.y = -4.2
         a.z = 3.1
         a.w = 1.0
         a is a point
         a is not a vector

Scenario: point x y z creates tuple with w = 1
    Given p <- point (4.3, -4.2, 3.1)
    Then p = tuple (4.3, -4.2, 3.1, 1.0)
-}
prop_Point :: TupleInput -> Bool
prop_Point (x, y, z, w) = isPoint t && predicate && not (isVector t)
     where predicate =
                   let wPredicate = getW t == 1
                       xPredicate = getX t == x
                       yPredicate = getY t == y
                       zPredicate = getZ t == z
                    in all (==True) [wPredicate, xPredicate, yPredicate, zPredicate]
           t = point (x,y,z)
{-
Scenario: A tuple with w = 0.0 is a vector
    Given a <- tuple (4.3, -4.2, 3.1, 1.0)
    Then a.x = 4.3
         a.y = -4.2
         a.z = 3.1
         a.w = 0.0
         a is not a point
         a is a vector
Scenario: vector (x,y,z) creates tuple with w = 0.0
    Given p <- vector (4.3, -4.2, 3.1)
    Then p = tuple (4.3, -4.2, 3.1, 0.0)
-}
prop_Vector :: TupleInput -> Bool
prop_Vector (x, y, z, w) = isVector t && predicate && not (isPoint t)
 where predicate =
                   let wPredicate = getW t == 0
                       xPredicate = getX t == x
                       yPredicate = getY t == y
                       zPredicate = getZ t == z
                    in all (==True) [wPredicate, xPredicate, yPredicate, zPredicate]
       t = vector (x,y,z)

prop_Tuple_Equal :: TupleInput -> TupleInput -> Bool
prop_Tuple_Equal (x1, y1, z1, w1) (x2, y2, z2, w2) = let xPredicate = predicate x1 x2
                                                         yPredicate = predicate y1 y2
                                                         zPredicate = predicate z1 z2
                                                         wPredicate = predicate w1 w2
                                                      in if (all (==True) [xPredicate, yPredicate, zPredicate, wPredicate])
                                                           then tuple (x1,y1 ,z1 ,w1) == tuple (x2,y2 ,z2 ,w2)
                                                           else tuple (x1,y1 ,z1 ,w1) /= tuple (x2,y2 ,z2 ,w2)
 where predicate a b = (abs (a - b) < 0.00001)

prop_Tuple_Addition :: TupleInput -> TupleInput -> Bool
prop_Tuple_Addition (x1, y1, z1, w1) (x2, y2, z2, w2) = (t1 + t2) == predicate
    where t1 = tuple (x1,y1 ,z1 ,w1)
          t2 = tuple (x2,y2 ,z2 ,w2)
          predicate = tuple ((x1 + x2), (y1 + y2), (z1 + z2), (w1 + w2))

prop_Tuple_Subtraction :: TupleInput -> TupleInput -> Bool
prop_Tuple_Subtraction (x1, y1, z1, w1) (x2, y2, z2, w2) = (t1 - t2) == predicate
    where t1 = tuple (x1,y1 ,z1 ,w1)
          t2 = tuple (x2,y2 ,z2 ,w2)
          predicate = tuple ((x1 - x2), (y1 - y2), (z1 - z2), (w1 - w2))

prop_Tuple_Negate :: TupleInput -> Bool
prop_Tuple_Negate (x, y, z, w) = (negate t) == predicate
 where t = tuple (x,y,z,w)
       predicate = vector (0,0,0) - tuple (x,y,z,w)

prop_Tuple_Scalar_Multiplication :: TupleInput -> Double -> Bool
prop_Tuple_Scalar_Multiplication (x,y,z,w) s = t * (pure s) == predicate
 where t = tuple (x,y,z,w)
       predicate = tuple ((x * s), (y * s), (z * s) ,(w * s))

prop_Tuple_Scalar_Division :: TupleInput -> Double -> Bool
prop_Tuple_Scalar_Division (x,y,z,w) s = skipIfNull || (t / (pure s) == predicate)
 where skipIfNull = isNull x || isNull y || isNull z || isNull w && isNull s
       isNull a = a == 0
       t = tuple (x,y,z,w)
       predicate = tuple ((x / s), (y / s), (z / s), (w / s))

prop_Tuple_Magnitude :: TupleInput -> Bool
prop_Tuple_Magnitude (x,y,z,w) = magnitude v == predicate
    where v = vector (x ,y ,z)
          predicate = sqrt ((x^2) + (y^2) + (z^2))

prop_Tuple_Normalize :: TupleInput -> Bool
prop_Tuple_Normalize (x,y,z,w) = skipIf || normalize v == predicate
    where skipIf = isNullAndNegative x || isNullAndNegative y || isNullAndNegative z || isNullAndNegative w
          isNullAndNegative a = a <= 0
          v = tuple (x,y,z,w)
          predicate = tuple ((x / magnitude v) ,(y / magnitude v) ,(z / magnitude v) ,(w / magnitude v))

prop_Tuple_Dot_Product :: TupleInput -> TupleInput -> Bool
prop_Tuple_Dot_Product (x1,y1,z1,w1) (x2,y2,z2,w2) = dot t1 t2 == predicate
    where t1 = tuple (x1,y1 ,z1 ,w1)
          t2 = tuple (x2,y2 ,z2 ,w2)
          predicate = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)

prop_Vector_Cross_Product :: TupleInput -> TupleInput -> Bool
prop_Vector_Cross_Product (x1,y1,z1,_) (x2,y2,z2,_) = cross t1 t2 == predicate
  where t1 = vector (x1 ,y1 ,z1)
        t2 = vector (x2,y2,z2)
        predicate = vector (((y1 * z2) - (z1 * y2)) ,((z1 * x2) - (x1 * z2)), ((x1 * y2) - (y1 * x2)))
