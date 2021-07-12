module Main where
import Criterion.Main
import Tuples
import Matrices
main = defaultMain [
  (bgroup "tuples" [
        bench "addition" $ (whnf ((+) (tuple 3 3 3 3)) (tuple 2 2 2 2))
        , bench "subtraction" $ (whnf ((-) (tuple 3 3 3 3)) (tuple 2 2 2 2))
        , bench "multiplication" $ (whnf ((*) (tuple 3 3 3 3)) (tuple 2 2 2 2))
        , bench "division" $ (whnf ((/) (tuple 3 3 3 3)) (tuple 2 2 2 2))
        ]),
   (bgroup "matrices" [
         bench "determinant_Matrix4x4" $ (whnf (determinant) (matrix 4 4 (\(i,j) -> fromIntegral (i + j))))
         , bench "determinant_Matrix3x3" $ (whnf (determinant) (matrix 3 3 (\(i,j) -> fromIntegral (i + j))))
         , bench "determinant_Matrix2x2" $ (whnf (determinant) (matrix 2 2 (\(i,j) -> fromIntegral (i + j))))
   ])
  ]