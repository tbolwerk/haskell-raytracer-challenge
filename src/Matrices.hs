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
data Matrix a = Matrix { getNRows ::  !Row
                       , getNCols ::  !Column                        
                       , getArray ::  !(Array (Row, Column) a)
                         }
instance Show a => Show (Matrix a) where
    show m@(Matrix r c _) = "Matrix" ++ show r ++ "x" ++ show c ++ foldMap (\xs -> '\n' : show xs) (matrixList m)

instance Eq (Matrix Double) where
    a == b = predicate
        where predicate = (getNCols a) == (getNCols b) && 
                          (getNRows a) == (getNRows b) &&
                          (all (==True) (zipWith compare (elems (getArray a)) (elems (getArray b))))
                   where compare a b = abs (a - b) < Tuples.epsilon

instance (Num a, Fractional a) => Num (Matrix a) where
   a * b = listToMatrix (getNRows a) (getNCols a) [ celCalc a b j i | j <- [0..getNCols a-1], i <- [0..getNRows a-1]]

instance Functor Matrix where
    fmap f m = listToMatrix (getNRows m) (getNCols m) (map f (elems $ getArray m))

instance Applicative Matrix where
    pure x = matrix 4 4 (\_ -> x)
    (<*>) = error "not defined"

celCalc :: (Num a, Fractional a) => Matrix a -> Matrix a -> Int -> Int -> a
celCalc a b i j = sum (zipWith (*) (getRow a i 0) (getCol b j 0))

-- getCol :: Matrix a -> Int -> Int -> [a]
-- getCol m currentIndex index | bound /= index -1 = (arr ! (index,currentIndex)) : getCol m currentIndex (index+1)
getCol :: Matrix a -> Int -> Int -> [a]
getCol m colNumber index | bound /= index -1 = (arr ! (index, colNumber)) : getCol m colNumber (index+1)
                         | otherwise = []
    where arr = getArray m
          (_,(_,bound)) = bounds arr

-- getRow :: Matrix a -> Int -> Int -> [a]
-- getRow m currentIndex index | bound /= index -1 = (arr ! (currentIndex,index)) : getRow m currentIndex (index+1)
getRow :: Matrix a -> Int -> Int -> [a]
getRow m rowNumber index | bound /= index -1 = (arr ! (rowNumber, index)) : getRow m rowNumber (index+1)
                         | otherwise = []
    where arr = getArray m
          (_,(bound,_)) = bounds arr



type Column = Int
type Row = Int

{- 
i increments row
j increments column
-}

matrix :: Row -> Column -> ((Row, Column) -> a) -> Matrix a
matrix r c f = Matrix r c (array ((0,0), (c-1, r-1)) [((i,j), f (i,j)) | i <- [0..r-1], j<- [0..c-1]])

get :: Matrix a -> (Row, Column) -> a
get m (i,j) = (getArray m) ! (i,j)

set :: Matrix a -> (Row, Column) -> a -> Matrix a
set m (i,j) v = Matrix  (getNRows m) (getNCols m) ((getArray m) // [((i, j), v)])

prettyPrint :: (Show a) => Matrix a -> IO ()
prettyPrint m = foldMap print (matrixList m)

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs
 | n > 0 = (take n xs) : (chunkOf n (drop n xs))
 | otherwise = error "Only positive numbers allowed"

horizontalIter :: Array (Int, Int) a -> [a]
horizontalIter array = [ array ! (x, y) | y <- [ly.. hy], x <- [lx .. hx]]
 where ((lx, ly), (hx, hy)) = bounds array

matrixList :: (Show a) => Matrix a -> [[a]]
matrixList m = (chunkOf (getNCols m) xs)
 where xs = (elems (getArray m))

listToMatrix :: Row -> Column -> [a] -> Matrix a
listToMatrix r c xs = Matrix r c (listArray ((0,0),(br,bc)) xs)
 where br = r -1
       bc = r -1

listToMatrix' :: [a] -> Matrix a
listToMatrix' xs = matrix bound bound (\(i, j) -> (((chunkOf bound xs) !! j) !! i)) 
 where bound = round (sqrt (fromIntegral (length xs)))

matrixVectorMultiply :: (Num a, Floating a) => Matrix a -> Tuple a -> Tuple a
matrixVectorMultiply m t =  listToTuple (map (\row -> dot row t) [ listToTuple (getRow m i 0) | i <- [0..getNRows m -1]])

listToTuple :: Num a => [a] -> Tuple a
listToTuple (x:y:z:w:[]) = tuple x y z w

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
          b = array ! (0,1)
          c = array ! (0,2)
determinant m@(Matrix 4 4 array) = a * a0 + b * a1 + c * a2 + d * a3
   where a0 = cofactor m 0 0
         a1 = cofactor m 0 1
         a2 = cofactor m 0 2
         a3 = cofactor m 0 3
         a = array ! (0,0)
         b = array ! (0,1)
         c = array ! (0,2)
         d = array ! (0,3)

submatrix :: (Num a, Show a) => Matrix a -> Row -> Column -> Matrix a
submatrix m i j = listToMatrix (getNRows m - 1) (getNCols m - 1) $ concatMap (\x -> take j x ++ drop (1 + j) x) removeRow
 where removeRow = take i (matrixList m) ++ drop (1 + i) (matrixList m)

minor :: (Num a,Show a, Fractional a) => Matrix a -> Row -> Column -> a
minor m i j = determinant (submatrix m i j)

cofactor :: (Num a, Show a, Fractional a)=> Matrix a -> Row -> Column -> a
cofactor m i j | even (i + j) = minor m i j
               | otherwise = negate (minor m i j) 
 
matrixScalarMultiply :: (Num a,Fractional a) => Matrix a -> a -> Matrix a
matrixScalarMultiply m s = Matrix (getNRows m) (getNCols m) (accum (\e a -> a e) (getArray m) [((i,j), (*s)) | j <- [0..getNCols m -1], i <- [0..getNRows m -1]])

{-
* note reverse order row column switched index on purpose.
-}
inverse :: (Num a, Show a, Fractional a) => Matrix a -> Matrix a
inverse m = transpose $ matrix (getNRows m) (getNCols m) (\(row, col) -> cofactor m row col / determinant m)

