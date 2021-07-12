{-# LANGUAGE FlexibleInstances #-}


{-
Part of chapter 3 of the raytracer challenge.

A lot of matrix calculus comes in handy here.

Used some references of:
https://semath.info/src/inverse-cofactor-ex4.html
https://www.wisfaq.nl/show3archive.asp?id=23989&j=2004
-}

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
    where check = (snd . snd . bounds . getArray)

instance Functor Matrix where
    fmap f m = listToMatrix (map f (elems $ getArray m))

instance Applicative Matrix where
    pure x = matrix 4 4 (\_ -> x)
    (<*>) = undefined

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
get m (i,j) = (getArray m) ! (j,i)

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

determinant :: (Show a, Num a, Fractional a) => Matrix a -> a
determinant (Matrix 2 2 array) = (a * d) - (b * c)
    where a = array ! (0,0)
          b = array ! (0,1)
          c = array ! (1,0)
          d = array ! (1,1)
determinant m@(Matrix 3 3 array) = a * efhi + b * dfgi + c * degh 
    where efhi = cofactor m 0 0
          dfgi = cofactor m 0 1
          degh = cofactor m 0 2
          a = array ! (0,0)
          b = array ! (1,0)
          c = array ! (2,0)
determinant m@(Matrix 4 4 array) = a * a0 + b * a1 + c * a2 + d * a3
   where a0 = cofactor m 0 0
         a1 = cofactor m 0 1
         a2 = cofactor m 0 2
         a3 = cofactor m 0 3
         a = array ! (0,0)
         b = array ! (1,0)
         c = array ! (2,0)
         d = array ! (3,0)

submatrix :: (Num a, Show a) => Matrix a -> Row -> Column -> Matrix a
submatrix m r c = listToMatrix $ concatMap (\x -> take c x ++ drop (1 + c) x) removeRow
 where removeRow = take r (matrixList m) ++ drop (1 + r) (matrixList m)

minor :: (Num a,Show a, Fractional a) => Matrix a -> Row -> Column -> a
minor m r c = determinant (submatrix m r c)

b = listToMatrix [3,5,0,2,-1,-7,6,-1,5]

cofactor :: (Num a, Show a, Fractional a)=> Matrix a -> Row -> Column -> a
cofactor m r c | even (r + c) = minor m r c 
                                  | otherwise = negate (minor m r c) 

a0 :: Matrix Double
a0 = listToMatrix [1,2,6,-5,8,-4,2,6,4]

matrixScalarMultiply :: (Num a,Fractional a) => Matrix a -> a -> Matrix a
matrixScalarMultiply m s = Matrix (getNRows m) (getNCols m) (accum (\e a -> a e) (getArray m) [((i,j), (*s)) | j <- [0..getNCols m -1], i <- [0..getNRows m -1]])

a10 :: Matrix Double
a10 = listToMatrix [-5,2,6,-8,1,-5,1,8,7,7,-6,-7,1,-3,7,4]

inverse :: (Num a, Show a, Fractional a) => Matrix a -> Matrix a
inverse m = matrix (getNRows m) (getNCols m) (\(i,j) -> (cofactor m i j) / determinant m)


inverseTest :: Bool
inverseTest = a4 == a4'
  where  c4 = a4 * b4
         a4' = c4 * (inverse b4)