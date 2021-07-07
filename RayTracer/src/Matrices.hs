module Matrices where
import Data.Array

data Matrix a = Matrix { getNRows :: Row,
                         getNCols :: Column,                        
                         getArray :: Array (Row, Column) a}
 deriving Show

type Column = Int
type Row = Int

{- 
i increments row
j increments column
-}

matrix :: Row -> Column -> ((Int, Int) -> a) -> Matrix a
matrix r c f = Matrix r c (array ((0,0), (c-1, r-1)) [((i,j), f (i,j)) | j <- [0..r-1], i <- [0..c-1]])

get :: Matrix a -> (Int, Int) -> a
get m (i,j) = (getArray m) ! (i,j)

set :: Matrix a -> (Int, Int) -> a -> Matrix a
set m (i,j) v = Matrix  (getNRows m) (getNCols m) ((getArray m) // [((i, j), v)])

myM = matrix 4 4 (\(i,j) -> case j of
                                0 -> fromIntegral i + 1
                                1 -> fromIntegral i + 5.5
                                2 -> fromIntegral i + 9
                                3 -> fromIntegral i + 13.5
                                _ -> fromIntegral i)


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