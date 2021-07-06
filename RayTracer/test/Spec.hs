import Test.QuickCheck as T
import Tuples

type TupleInput = (Double,Double,Double,Double)

main :: IO ()
main = do
     (T.quickCheck prop_Point)
     (T.quickCheck prop_Vector)
     (T.quickCheck prop_Tuple_Equal)
     (T.quickCheck prop_Tuple_Addition)
     (T.quickCheck prop_Tuple_Subtraction)

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
prop_Point (x, y, z, w) = Tuples.isPoint t && predicate && not (Tuples.isVector t)
     where predicate = 
                   let wPredicate = Tuples.getW t == 1
                       xPredicate = Tuples.getX t == x
                       yPredicate = Tuples.getY t == y
                       zPredicate = Tuples.getZ t == z
                    in all (==True) [wPredicate, xPredicate, yPredicate, zPredicate]
           t = Tuples.point x y z
{- 
Scenario: A tuple with w = 0.0 is a vector
    Given a <- tuple (4.3, -4.2, 3.1, 1.0)
    Then a.x = 4.3
         a.y = -4.2
         a.z = 3.1
         a.w = 0.0
         a is not a point
         a is a vector
Scenario: vector x y z creates tuple with w = 0.0
    Given p <- vector (4.3, -4.2, 3.1)
    Then p = tuple (4.3, -4.2, 3.1, 0.0)
-}
prop_Vector :: TupleInput -> Bool
prop_Vector (x, y, z, w) = Tuples.isVector t && predicate && not (Tuples.isPoint t)
 where predicate = 
                   let wPredicate = Tuples.getW t == 0
                       xPredicate = Tuples.getX t == x
                       yPredicate = Tuples.getY t == y
                       zPredicate = Tuples.getZ t == z
                    in all (==True) [wPredicate, xPredicate, yPredicate, zPredicate]
       t = Tuples.vector x y z

prop_Tuple_Equal :: TupleInput -> TupleInput -> Bool
prop_Tuple_Equal (x1, y1, z1, w1) (x2, y2, z2, w2) = let xPredicate = predicate x1 x2
                                                         yPredicate = predicate y1 y2
                                                         zPredicate = predicate z1 z2
                                                         wPredicate = predicate w1 w2
                                                      in if (all (==True) [xPredicate, yPredicate, zPredicate, wPredicate])
                                                           then tuple x1 y1 z1 w1 == tuple x2 y2 z2 w2
                                                           else tuple x1 y1 z1 w1 /= tuple x2 y2 z2 w2
 where predicate a b = (abs (a - b) < 0.00001)

prop_Tuple_Addition :: TupleInput -> TupleInput -> Bool
prop_Tuple_Addition (x1, y1, z1, w1) (x2, y2, z2, w2) = (t1 + t2) == predicate
    where t1 = Tuples.tuple x1 y1 z1 w1
          t2 = Tuples.tuple x2 y2 z2 w2
          predicate = Tuples.tuple (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)

prop_Tuple_Subtraction :: TupleInput -> TupleInput -> Bool
prop_Tuple_Subtraction (x1, y1, z1, w1) (x2, y2, z2, w2) = (t1 - t2) == predicate
    where t1 = Tuples.tuple x1 y1 z1 w1
          t2 = Tuples.tuple x2 y2 z2 w2
          predicate = Tuples.tuple (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)

