import Test.QuickCheck as T
import Test.HUnit
import Tuples
import Canvas
import Colors
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)



type TupleInput = (Double,Double,Double,Double)
type CanvasInput = (Int, Int)


main = defaultMain tests

tests = [
        testGroup "Tuples" [
                testProperty "prop_Point" prop_Point
                , testProperty "prop_Vector" prop_Vector
                , testProperty "prop_Tuple_Equal" prop_Tuple_Equal
                , testProperty "prop_Tuple_Addition" prop_Tuple_Addition
                , testProperty "prop_Tuple_Subtraction" prop_Tuple_Subtraction
                , testProperty "prop_Tuple_Negate" prop_Tuple_Negate
                , testProperty "prop_Tuple_Scalar_Multiplication" prop_Tuple_Scalar_Multiplication
                , testProperty "prop_Tuple_Scalar_Division" prop_Tuple_Scalar_Division
                , testProperty "prop_Tuple_Magnitude" prop_Tuple_Magnitude
                , testProperty "prop_Tuple_Normalize" prop_Tuple_Normalize
                , testProperty "prop_Tuple_Dot_Product" prop_Tuple_Dot_Product
                , testProperty "prop_Vector_Cross_Product" prop_Vector_Cross_Product
            ],
        testGroup "Canvas" [
                testProperty "prop_Canvas_Create" prop_Canvas_Create
                , testProperty "prop_Canvas_PixelAt" prop_Canvas_PixelAt
                , testProperty "prop_Canvas_WritePixel" prop_Canvas_WritePixel
                , testProperty "prop_Canvas_PPM_EndsWithNewLine" prop_Canvas_PPM_EndsWithNewLine
                , testCase "testCase1" testCase1
            ]
    ]

testCase1 = assertEqual "for (foo 3)," 4 4

     

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

prop_Tuple_Negate :: TupleInput -> Bool
prop_Tuple_Negate (x, y, z, w) = (negate t) == predicate
 where t = Tuples.tuple x y z w
       predicate = Tuples.vector 0 0 0 - Tuples.tuple x y z w

prop_Tuple_Scalar_Multiplication :: TupleInput -> Double -> Bool
prop_Tuple_Scalar_Multiplication (x,y,z,w) s = t * (pure s) == predicate
 where t = Tuples.tuple x y z w
       predicate = Tuples.tuple (x * s) (y * s) (z * s) (w * s)

prop_Tuple_Scalar_Division :: TupleInput -> Double -> Bool
prop_Tuple_Scalar_Division (x,y,z,w) s = skipIfNull || (t / (pure s) == predicate)
 where skipIfNull = isNull x || isNull y || isNull z || isNull w && isNull s
       isNull a = a == 0
       t = Tuples.tuple x y z w
       predicate = Tuples.tuple (x / s) (y / s) (z / s) (w / s)

prop_Tuple_Magnitude :: TupleInput -> Bool
prop_Tuple_Magnitude (x,y,z,w) = Tuples.magnitude v == predicate
    where v = Tuples.vector x y z
          predicate = sqrt ((x^2) + (y^2) + (z^2)) 

prop_Tuple_Normalize :: TupleInput -> Bool
prop_Tuple_Normalize (x,y,z,w) = skipIf || normalize v == predicate
    where skipIf = isNullAndNegative x || isNullAndNegative y || isNullAndNegative z || isNullAndNegative w
          isNullAndNegative a = a <= 0
          v = Tuples.tuple x y z w
          predicate = Tuples.tuple (x / Tuples.magnitude v) (y / Tuples.magnitude v) (z / Tuples.magnitude v) (w / Tuples.magnitude v)

prop_Tuple_Dot_Product :: TupleInput -> TupleInput -> Bool
prop_Tuple_Dot_Product (x1,y1,z1,w1) (x2,y2,z2,w2) = dot t1 t2 == predicate
    where t1 = Tuples.tuple x1 y1 z1 w1
          t2 = Tuples.tuple x2 y2 z2 w2
          predicate = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)

prop_Vector_Cross_Product :: TupleInput -> TupleInput -> Bool
prop_Vector_Cross_Product (x1,y1,z1,_) (x2,y2,z2,_) = Tuples.cross t1 t2 == predicate
  where t1 = Tuples.vector x1 y1 z1 
        t2 = Tuples.vector x2 y2 z2
        predicate = Tuples.vector ((y1 * z2) - (z1 * y2)) ((z1 * x2) - (x1 * z2)) ((x1 * y2) - (y1 * x2)) 


prop_Canvas_Create :: CanvasInput -> Bool
prop_Canvas_Create (width, height) = ((getWidth c == width -1) && 
                                     (getHeight c == height -1) &&
                                     predicate)
  where c = canvas width height
        isBlack col = getRed col == 0 && getGreen col == 0 && getBlue col == 0
        predicate = all (\x -> isBlack (getColor x)) (pixels c)

prop_Canvas_PixelAt :: CanvasInput -> Bool
prop_Canvas_PixelAt (width, height) = (width <= 0 || height <= 0) || isBlack (getColor (pixelAt c 0 0))
    where c = canvas width height
          isBlack col = getRed col == 0 && getGreen col == 0 && getBlue col == 0

prop_Canvas_WritePixel :: CanvasInput -> Bool
prop_Canvas_WritePixel (width, height) = (width <= 0 || height <= 0) || (black /= actualColor) && (actualColor == red)
    where c = canvas width height
          nc = writePixel c 0 0 red 
          red = color 1 0 0 0
          black = color 0 0 0 0
          actualColor = getColor (pixelAt nc 0 0)

prop_Canvas_PPM_EndsWithNewLine :: CanvasInput -> Bool
prop_Canvas_PPM_EndsWithNewLine (width, height) = (width <= 0 || height <= 0) || predicate
 where c = canvas width height 
       predicate = last (canvasToString c) == '\n'


