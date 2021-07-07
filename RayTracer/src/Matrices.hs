{-# LANGUAGE FlexibleInstances #-}
module Matrices where
import Data.Array
import Tuples

data Matrix a = Matrix { getNRows :: Row,
                         getNCols :: Column,                        
                         getArray :: Array (Row, Column) a}
 deriving Show

instance Eq (Matrix Double) where
    a == b = predicate
        where predicate = (getNCols a) == (getNCols b) && 
                          (getNRows a) == (getNRows b) &&
                          (all (==True) (zipWith compare (elems (getArray a)) (elems (getArray b))))
                   where compare a b = abs (a - b) < Tuples.epsilon

instance (Num a, Fractional a) => Num (Matrix a) where
   a * b = listToMatrix [ celCalc a b i j | i <- [0..getNRows a-1], j <- [0..getNCols a-1]]

instance Functor Matrix where
    fmap f m = listToMatrix (map f (elems $ getArray m))

celCalc :: (Num a, Fractional a) => Matrix a -> Matrix a -> Int -> Int -> a
celCalc a b i j = sum (zipWith (*) (getRow a i 0) (getCol b j 0))

getRow :: Matrix a -> Int -> Int -> [a]
getRow m rowNumber index | bound /= index -1 = (arr ! (index,rowNumber)) : getRow m rowNumber (index+1)
                         | otherwise = []
    where arr = getArray m
          (_,(_,bound)) = bounds arr

getCol :: Matrix a -> Int -> Int -> [a]
getCol m colNumber index | bound /= index -1 = (arr ! (colNumber,index)) : getCol m colNumber (index+1)
                         | otherwise = []
    where arr = getArray m
          (_,(_,bound)) = bounds arr

type Column = Int
type Row = Int

{- 
i increments row
j increments column
-}

matrix :: Row -> Column -> ((Row, Column) -> a) -> Matrix a
matrix r c f = Matrix r c (array ((0,0), (c-1, r-1)) [((i,j), f (i,j)) | j <- [0..r-1], i <- [0..c-1]])


get :: Matrix a -> (Row, Column) -> a
get m (i,j) = (getArray m) ! (i,j)

set :: Matrix a -> (Row, Column) -> a -> Matrix a
set m (i,j) v = Matrix  (getNRows m) (getNCols m) ((getArray m) // [((i, j), v)])

myM :: Matrix Double
myM = matrix 4 4 (\(i,j) -> case j of
                                0 -> fromIntegral i + 1
                                1 -> fromIntegral i + 5.5
                                2 -> fromIntegral i + 9
                                3 -> fromIntegral i + 13.5
                                _ -> fromIntegral i)

myM' :: Matrix Double
myM' = matrix 4 4 (\(i,j) -> fromIntegral i)

prettyPrint :: (Show a) => Matrix a -> IO ()
prettyPrint m = foldMap print (matrixList m)

matrixList :: (Show a) => Matrix a -> [[a]]
matrixList m = (chunkOf (getNCols m) xs)
 where xs = (horizontalIter (getArray m))

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs
 | n > 0 = (take n xs) : (chunkOf n (drop n xs))
 | otherwise = error "Only positive numbers allowed"

horizontalIter :: Array (Int, Int) a -> [a]
horizontalIter array = [ array ! (x, y) | y <- [ly.. hy], x <- [lx .. hx]]
 where ((lx, ly), (hx, hy)) = bounds array

listToMatrix :: [a] -> Matrix a
listToMatrix xs = matrix bound bound (\(i, j) -> (((chunkOf bound xs) !! j) !! i)) 
 where bound = round (sqrt (fromIntegral (length xs)))


a4 :: Matrix Double
a4 = listToMatrix [1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2]
b4 :: Matrix Double
b4 = listToMatrix [-2, 1,2 ,3,3,2,1,-1,4,3,6,5,1,2,7,8]

a3 :: Matrix Double
a3 = listToMatrix [7,8,9,8,7,6,5,4,3,2]

b3 :: Matrix Double
b3 = listToMatrix [-2, -1,-2 ,3,3,2,-1,-1,-4]

matrixVectorMultiply :: (Num a, Floating a) => Matrix a -> Tuple a -> Tuple a
matrixVectorMultiply m t =  listToTuple (map (\row -> dot row t) [ listToTuple (getRow m i 0) | i <- [0..getNCols m -1]])

listToTuple :: Num a => [a] -> Tuple a
listToTuple (x:y:z:w:[]) = tuple x y z w


calc = matrixVectorMultiply a1 b1

a1 = listToMatrix [1,2,3,4,2,4,4,2,8,6,4,1,0,0,0,1]
b1 = listToTuple [1,2,3,1]

identityMatrix :: Matrix Double
identityMatrix = matrix 4 4 (\(i,j) -> if i == j then 1.0 else 0.0)

transpose :: Matrix a -> Matrix a 
transpose m = matrix (getNRows m) (getNCols m) (\(i,j) -> getArray m ! (j,i))

determinant :: (Show a, Num a) => Matrix a -> a
determinant (Matrix 2 2 array) = (a * d) - (b * c)
    where a = array ! (0,0)
          b = array ! (0,1)
          c = array ! (1,0)
          d = array ! (1,1)

submatrix :: (Num a, Show a) => Matrix a -> Row -> Column -> Matrix a
submatrix m r c = listToMatrix $ concat (map (\x -> take c x ++ drop (1 + c) x) removeRow)
 where removeRow = take r (matrixList m) ++ drop (1 + r) (matrixList m)

minor :: (Num a,Show a) => Matrix a -> Row -> Column -> a
minor m@(Matrix 3 3 array) r c = determinant (submatrix m r c)

b = listToMatrix [3,5,0,2,-1,-7,6,-1,5]
