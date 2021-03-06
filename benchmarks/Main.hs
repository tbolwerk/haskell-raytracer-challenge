module Main where
import Criterion.Main
import Tuples
import Matrices
import qualified LinearAlgebra as L



tupleA = tuple (3, 3, 3, 3)
tupleB = tuple (2, 2, 2, 2)
vectorA = vector (3, 3, 3)
vectorB = vector (2, 2, 2)

matrixA4 :: Matrix Double
matrixA4 = listToMatrix 4 4 [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
matrixB4 :: Matrix Double
matrixB4 = listToMatrix 4 4 [-2, 1,2 ,3,3,2,1,-1,4,3,6,5,1,2,7,8]
matrixA3 :: Matrix Double
matrixA3 = listToMatrix 3 3 [7,8,9,8,7,6,5,4,3,2]
matrixB3 :: Matrix Double
matrixB3 = listToMatrix 3 3 [-2, -1,-2 ,3,3,2,-1,-1,-4]
matrixA2 :: Matrix Double
matrixA2 = listToMatrix 2 2 [7,8,9,8,7]
matrixB2 :: Matrix Double
matrixB2 = listToMatrix 2 2 [-2, -1,-2 ,3]

tupleA' = L.tuple (3, 3, 3, 3)
tupleB' = L.tuple (2, 2, 2, 2)
vectorA' = L.vector (3, 3, 3)
vectorB' = L.vector (2, 2, 2)

matrixA4' ::  L.Matrix Double
matrixA4' =  L.listToMatrix 4 4 [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
matrixB4' ::  L.Matrix Double
matrixB4' =  L.listToMatrix 4 4 [-2, 1,2 ,3,3,2,1,-1,4,3,6,5,1,2,7,8]
matrixA3' ::  L.Matrix Double
matrixA3' =  L.listToMatrix 3 3 [7,8,9,8,7,6,5,4,3,2]
matrixB3' ::  L.Matrix Double
matrixB3' =  L.listToMatrix 3 3 [-2, -1,-2 ,3,3,2,-1,-1,-4]
matrixA2' ::  L.Matrix Double
matrixA2' =  L.listToMatrix 2 2 [7,8,9,8,7]
matrixB2' ::  L.Matrix Double
matrixB2' = L.listToMatrix 2 2 [-2, -1,-2 ,3]

main = defaultMain [
  (bgroup "tuples" [
        bench "addition" $ (whnf ((+) (tupleA)) (tupleB))
        , bench "subtraction" $ (whnf ((-) (tupleA)) (tupleB))
        , bench "multiplication" $ (whnf ((*) (tupleA)) (tupleB))
        , bench "division" $ (whnf ((/) (tupleA)) (tupleB))
        , bench "dot" $ (whnf ((dot) (tupleA)) (tupleB))
        , bench "cross" $ (whnf ((cross) (vectorA)) (vectorB))
        , bench "magnitude" $ (whnf (magnitude) (tupleB))
        , bench "normalize" $ (whnf (normalize) (tupleB))
        , bench "scalarMultiplication" $ (whnf ((*) tupleA) (pure 2))
        ]),
   (bgroup "matrices" [
         bench "multiplication_Matrix4x4" $ (whnf ((*) matrixA4) (matrixB4))
         , bench "multiplication_Matrix3x3" $ (whnf ((*) matrixA3) (matrixB3))
         , bench "multiplication_Matrix2x2" $ (whnf ((*) matrixA2) (matrixB2))
         , bench "determinant_Matrix4x4" $ (whnf (determinant) (matrixB4))
         , bench "determinant_Matrix3x3" $ (whnf (determinant) (matrixB3))
         , bench "determinant_Matrix2x2" $ (whnf (determinant) (matrixB2))
         , bench "inverse_Matrix4x4" $ (whnf (inverse) (matrixB4))
         , bench "inverse_Matrix3x3" $ (whnf (inverse) (matrixB3))
         , bench "inverse_Matrix2x2" $ (whnf (inverse) (matrixB2)) 
         , bench "matrixVectorMultiply" $ (whnf (matrixVectorMultiply matrixB4) (tupleB))
         , bench "listToMatrix" $ whnf (listToMatrix 4 4) [1..16]
         , bench "matrixToList" $ whnf matrixList matrixA4
         , bench "horizontalIter" $ whnf horizontalIter (getArray matrixA4)
   ]),
     (bgroup "linear algebra" [
         bench "multiplication_Matrix4x4" $ (whnf ((*) matrixA4') (matrixB4'))
         , bench "multiplication_Matrix3x3" $ (whnf ((*) matrixA3') (matrixB3'))
         , bench "multiplication_Matrix2x2" $ (whnf ((*) matrixA2') (matrixB2'))
         , bench "determinant_Matrix4x4" $ (whnf (L.determinant) (matrixB4'))
         , bench "determinant_Matrix3x3" $ (whnf (L.determinant) (matrixB3'))
         , bench "determinant_Matrix2x2" $ (whnf (L.determinant) (matrixB2'))
         , bench "inverse_Matrix4x4" $ (whnf (L.inverse) (matrixB4'))
         , bench "inverse_Matrix3x3" $ (whnf (L.inverse) (matrixB3'))
         , bench "inverse_Matrix2x2" $ (whnf (L.inverse) (matrixB2')) 
         , bench "matrixVectorMultiply" $ (whnf (L.matrixVectorMultiply matrixB4') (tupleB'))
         , bench "listToMatrix" $ whnf (L.listToMatrix 4 4) [1..16]
   ])
  ]