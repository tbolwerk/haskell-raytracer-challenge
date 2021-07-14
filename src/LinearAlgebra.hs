-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE StandaloneKindSignatures #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- module LinearAlgebra where
-- -- import qualified Prelude as P

-- type R = Double
-- type Z = Integer

-- type Scalar a = R
-- type Vector a = [R]
-- type Matrix a = [[R]]

-- data family LinearAlgebra a
-- data instance LinearAlgebra R = Scalar R
-- data instance LinearAlgebra [R] = Vector [(Scalar R)]
-- data instance LinearAlgebra [[R]] = Matrix [(Vector R)]

-- nonsense :: LinearAlgebra R -> R
-- nonsense (Scalar a) = a

-- nonsense' :: LinearAlgebra [R] -> [R]
-- nonsense' (Vector a) =  a

-- nonsense'' :: LinearAlgebra [[R]] -> [[R]]
-- nonsense'' (Matrix a) = a

-- matrixVectorMultiply :: LinearAlgebra [[R]] -> LinearAlgebra [R] -> LinearAlgebra [R]
-- matrixVectorMultiply (Matrix a) (Vector b) = Vector [1..4]

-- ex1 = nonsense (Scalar 1)
-- ex2 = nonsense' (Vector [1..4])
-- ex3 = nonsense'' (Matrix [[1..4],[1..4],[1..4],[1..4]])

-- instance Num (Scalar a) where
--     (Scalar a) + (Scalar b) = Scalar (a + b)

-- instance Num a => Num (LinearAlgebra a) where
--  (+) a b = Scalar (a + b)
--  (*) a b = a * b

 