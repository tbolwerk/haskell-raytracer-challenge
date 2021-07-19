module MatricesTest where
import           Matrices
import           Test.HUnit
import qualified Test.QuickCheck as T
m1 :: Matrix Double
m1 = (Matrices.listToMatrix 4 4 [1,2,3,4,5.5,6.5,7.5,8.5,9,10,11,12,13.5,14.5,15.5,16.5])
m2 :: Matrix Double
m2 = matrix 4 4 (\(i,j) -> case i of
                                0 -> fromIntegral j + 1
                                1 -> fromIntegral j + 5.5
                                2 -> fromIntegral j + 9
                                3 -> fromIntegral j + 13.5
                                _ -> fromIntegral j)

a4 :: Matrix Double
a4 = listToMatrix 4 4 [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
b4 :: Matrix Double
b4 = listToMatrix 4 4 [-2, 1,2 ,3,3,2,1,-1,4,3,6,5,1,2,7,8]
a3 :: Matrix Double
a3 = listToMatrix 3 3 [7,8,9,8,7,6,5,4,3,2]
b3 :: Matrix Double
b3 = listToMatrix 3 3 [-2, -1,-2 ,3,3,2,-1,-1,-4]






testCase36 = assertEqual "given matrix M1[0,0] = 1" 1 (get m1 (0,0))
testCase37 = assertEqual "given matrix M1[0,3] = 4" 4 (get m1 (0,3))
testCase38 = assertEqual "given matrix M1[1,0] = 5.5" 5.5 (get m1 (1,0))
testCase39 = assertEqual "given matrix M1[1,2] = 7.5" 7.5 (get m1 (1,2))
testCase40 = assertEqual "given matrix M1[2,2] = 11" 11 (get m1 (2,2))
testCase41 = assertEqual "given matrix M1[3,0] = 13.5" 13.5 (get m1 (3,0))
testCase42 = assertEqual "given matrix M1[3,2] = 15.5" 15.5 (get m1 (3,2))

testCase43 = assertEqual "given matrix M2[0,0] = 1" 1 (get m2 (0,0))
testCase44 = assertEqual "given matrix M2[0,3] = 4" 4 (get m2 (0,3))
testCase45 = assertEqual "given matrix M2[1,0] = 5.5" 5.5 (get m2 (1,0))
testCase46 = assertEqual "given matrix M2[1,2] = 7.5" 7.5 (get m2 (1,2))
testCase47 = assertEqual "given matrix M2[2,2] = 11" 11 (get m2 (2,2))
testCase48 = assertEqual "given matrix M2[3,0] = 13.5" 13.5 (get m2 (3,0))
testCase49 = assertEqual "given matrix M2[3,2] = 15.5" 15.5 (get m2 (3,2))
testCase50 = assertEqual "(a4 * b4) * (inverse b4) = a4'" a4 a4'
  where  c4 = a4 * b4
         a4' = c4 * (inverse b4)

testCase52 = assertEqual "Matrix4x4 A" ((Matrices.listToMatrix 4 4 ([0.21805,0.45113,0.24060,-0.04511,-0.80827,-1.45677,-0.44361,0.52068,-0.07895,-0.22368,-0.05263,0.19737,-0.52256,-0.81391,-0.30075,0.30639] :: [Double]))) (inverse $ Matrices.listToMatrix 4 4 [-5,2,6,-8,1,-5,1,8,7,7,-6,-7,1,-3,7,4])
