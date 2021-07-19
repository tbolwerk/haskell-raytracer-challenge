module LinearAlgebraTest where
import           LinearAlgebra
import           Test.HUnit
import qualified Test.QuickCheck as T
m1 :: Matrix Double
m1 = (fromListM [1,2,3,4,5.5,6.5,7.5,8.5,9,10,11,12,13.5,14.5,15.5,16.5])
m2 :: Matrix Double
m2 = matrix [tuple (1,2,3,4), tuple (5.5,6.5,7.5,8.5), tuple (9.0,10.0,11.0,12.0), tuple (13.5,14.5,15.5,16.5)]

a4 :: Matrix Double
a4 = fromListM  [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
b4 :: Matrix Double
b4 =  fromListM [-2, 1,2 ,3,3,2,1,-1,4,3,6,5,1,2,7,8]
a3 :: Matrix Double
a3 = fromListM  [7,8,9,8,7,6,5,4,3,2]
b3 :: Matrix Double
b3 = fromListM  [-2, -1,-2 ,3,3,2,-1,-1,-4]

testCase53 = assertEqual "given matrix M1[0,0] = 1" 1 (get m1 (0,0))
testCase54 = assertEqual "given matrix M1[0,3] = 4" 4 (get m1 (0,3))
testCase55 = assertEqual "given matrix M1[1,0] = 5.5" 5.5 (get m1 (1,0))
testCase56 = assertEqual "given matrix M1[1,2] = 7.5" 7.5 (get m1 (1,2))
testCase57 = assertEqual "given matrix M1[2,2] = 11" 11 (get m1 (2,2))
testCase58 = assertEqual "given matrix M1[3,0] = 13.5" 13.5 (get m1 (3,0))
testCase59 = assertEqual "given matrix M1[3,2] = 15.5" 15.5 (get m1 (3,2))

testCase60 = assertEqual "given matrix M2[0,0] = 1" 1 (get m2 (0,0))
testCase61 = assertEqual "given matrix M2[0,3] = 4" 4 (get m2 (0,3))
testCase62 = assertEqual "given matrix M2[1,0] = 5.5" 5.5 (get m2 (1,0))
testCase63 = assertEqual "given matrix M2[1,2] = 7.5" 7.5 (get m2 (1,2))
testCase64 = assertEqual "given matrix M2[2,2] = 11" 11 (get m2 (2,2))
testCase65 = assertEqual "given matrix M2[3,0] = 13.5" 13.5 (get m2 (3,0))
testCase66 = assertEqual "given matrix M2[3,2] = 15.5" 15.5 (get m2 (3,2))
testCase67 = assertEqual "(a4 * b4) * (inverse b4) = a4'" a4 a4'
  where  c4 = a4 * b4
         a4' = c4 * (inverse b4)

testCase68 = assertEqual "Matrix4x4 A" ((fromListM ([0.21805,0.45113,0.24060,-0.04511,-0.80827,-1.45677,-0.44361,0.52068,-0.07895,-0.22368,-0.05263,0.19737,-0.52256,-0.81391,-0.30075,0.30639] :: [Double]))) (inverse $ fromListM [-5,2,6,-8,1,-5,1,8,7,7,-6,-7,1,-3,7,4])
