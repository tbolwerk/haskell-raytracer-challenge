import Test.QuickCheck as T
import Tuples
main :: IO ()
main = do
     (T.quickCheck prop_Point)
     (T.quickCheck prop_Vector)

{- 
Scenario: A tuple with w = 1.0 is a point
    Given a <- tuple (4.3, -4.2, 3.1, 1.0)
    Then a.x = 4.3
-}
prop_Point :: Double -> Double -> Double -> Double -> Bool
prop_Point x y z w = Tuples.isPoint t && predicate && not (Tuples.isVector t)
     where predicate = 
                   let wPredicate = Tuples.getW t == 1
                       xPredicate = Tuples.getX t == x
                       yPredicate = Tuples.getY t == y
                       zPredicate = Tuples.getZ t == z
                    in all (==True) [wPredicate, xPredicate, yPredicate, zPredicate]
           t = Tuples.point x y z
 
prop_Vector :: Double -> Double -> Double -> Double -> Bool
prop_Vector x y z w = Tuples.isVector t && predicate && not (Tuples.isPoint t)
 where predicate = 
                   let wPredicate = Tuples.getW t == 0
                       xPredicate = Tuples.getX t == x
                       yPredicate = Tuples.getY t == y
                       zPredicate = Tuples.getZ t == z
                    in all (==True) [wPredicate, xPredicate, yPredicate, zPredicate]
       t = Tuples.vector x y z