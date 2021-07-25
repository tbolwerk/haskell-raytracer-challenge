{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module LinearAlgebra where
import qualified Data.List as L

epsilon :: Double
epsilon = 0.00001

-- backward compatibility
matrixVectorMultiply m v = (*|>) m v
listToMatrix _ _ m = fromListM m
identityMatrix = matrix [ tuple (1,0,0,0)
                         ,tuple (0,1,0,0)
                         ,tuple (0,0,1,0)
                         ,tuple (0,0,0,1)]

class VectorMath a where
    magnitude :: a -> Double
    normalize :: a -> a
    dot :: a -> a -> Double
    cross :: a -> a -> a

type TupleInput = (Double, Double, Double, Double)
type VectorInput = (Double,Double,Double)
type PointInput = VectorInput

reflect :: (Tuple Double, Tuple Double) -> Tuple Double
reflect (v,n) = v - n * pure (2 * dot v n)

data Tuple a = Tuple { getX :: a, getY :: a, getZ :: a, getW :: a } | Tuple3 {getX :: a, getY :: a, getZ :: a} | Tuple2 {getX :: a, getY :: a}

getTuple :: Tuple a -> (a,a,a,a)
getTuple (Tuple a b c d) = (a,b,c,d)
getTuple3 :: Tuple a -> (a,a,a)
getTuple3 (Tuple3 a b c) = (a,b,c)
getTuple2 :: Tuple a -> (a,a)
getTuple2 (Tuple2 a b) = (a,b)

getTupleList :: Tuple a -> [a]
getTupleList (Tuple a b c d) = a:b:c:d:[]
getTupleList (Tuple3 a b c)  = a:b:c:[]
getTupleList (Tuple2 a b)    = a:b:[]

instance Foldable Tuple where
 foldMap f (Tuple a b c d) = f a <> f b <> f c <> f d
 foldMap f (Tuple3 a b c ) = f a <> f b <> f c
 foldMap f (Tuple2 a b   ) = f a <> f b

instance Show a => Show (Tuple a) where
 show (Tuple a b c d) = "Tuple (" ++ show a ++", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ")"
 show (Tuple3 a b c) = "Tuple3 (" ++ show a ++", " ++ show b ++ ", " ++ show c ++ ")"
 show (Tuple2 a b ) = "Tuple2 (" ++ show a ++", " ++ show b ++ ", " ++ ")"

tuple :: TupleInput -> Tuple Double
tuple (x, y, z, w) = Tuple x y z w
tuple3 :: (Double, Double, Double) -> Tuple Double
tuple3 (x,y,z) = Tuple3 x y z
tuple2 :: (Double,Double) -> Tuple Double
tuple2 (x,y) = Tuple2 x y

vector :: VectorInput -> Tuple Double
vector (x,y,z) = Tuple x y z 0

point :: PointInput -> Tuple Double
point (x,y,z) = Tuple x y z 1

instance VectorMath (Tuple Double) where
 magnitude (Tuple x y z w) = sqrt ((x^2) + (y^2) + (z^2) + (w^2))
 magnitude (Tuple3 x y z)  = sqrt ((x^2) + (y^2) + (z^2) )
 magnitude (Tuple2 x y)    = sqrt ((x^2) + (y^2))
 normalize t = fmap (\a -> a / magnitude t) t
 dot (Tuple x1 y1 z1 w1) (Tuple x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)
 dot (Tuple3 x1 y1 z1) (Tuple3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
 dot (Tuple2 x1 y1) (Tuple2 x2 y2) = (x1 * x2) + (y1 * y2)
 cross (Tuple x1 y1 z1 w1) (Tuple x2 y2 z2 w2) = vector (((y1 * z2) - (z1 * y2)), ((z1*x2) - (x1 *z2)), ((x1*y2) - (y1*x2)))

instance Eq (Tuple Double) where
 Tuple x1 y1 z1 w1 == Tuple x2 y2 z2 w2 = (compare x1 x2) && (compare y1 y2) && (compare z1 z2) && (compare w1 w2)
  where compare a b = abs (a - b) < epsilon

instance Functor Tuple where
 fmap f (Tuple x y z w) = Tuple (f x) (f y) (f z) (f w)
 fmap f (Tuple3 x y z)  = Tuple3 (f x) (f y) (f z)
 fmap f (Tuple2 x y)    = Tuple2 (f x) (f y)

instance Applicative Tuple where
 pure x = Tuple x x x x
 (Tuple x1 y1 z1 w1) <*> (Tuple x2 y2 z2 w2) = Tuple (x1 x2) (y1 y2) (z1 z2) (w1 w2)

instance  Num (Tuple Double) where
 a + b = (fmap (+) a) <*> b
 a - b = (fmap (-) a) <*> b
 negate a = vector (0, 0, 0) - a
 a * b = (fmap (*) a) <*> b

instance Fractional (Tuple Double) where
 a / b = (fmap (/) a) <*> b

type Row = Int
type Column = Int

class MatrixMath a where
 determinant :: a -> Double
 transpose :: a -> a
 inverse :: a -> a
 cofactor :: a -> Row -> Column -> Double
 minor :: a -> Row -> Column -> Double
 submatrix :: a -> Row -> Column -> a
 (*|>) :: a -> Tuple Double -> Tuple Double
 (*|>>) :: a -> Double -> a

 infixl 7 *|>
 infixl 7 *|>>

instance MatrixMath (Matrix Double) where
 matrix *|>> s = matrix * pure s
 matrix *|> tuple = listToTuple (map (dot tuple) (row matrix))
 inverse m@(Matrix4 _) = matrix (map listToTuple (chunkOf 4 ([(cofactor m i j / determinant m) | j <- [0..3], i <- [0..3]])))
 inverse m@(Matrix3 _) = matrix (map listToTuple (chunkOf 3 ([(cofactor m i j / determinant m) | j <- [0..2], i <- [0..2]])))
 inverse m@(Matrix2 _) = matrix (map listToTuple (chunkOf 2 ([(cofactor m i j / determinant m) | j <- [0..1], i <- [0..1]])))
 determinant (Matrix2 array) = (a * d) - (b * c)
    where Tuple2 a _ = array !! 0
          Tuple2 _ b = array !! 0
          Tuple2 c _ = array !! 1
          Tuple2 _ d  = array !! 1
 determinant m@(Matrix3 array) = a * efhi + b * dfgi + c * degh
    where efhi = cofactor m 0 0
          dfgi = cofactor m 0 1
          degh = cofactor m 0 2
          Tuple3 a _ _ = array !! 0
          Tuple3 _ b _ = array !! 0
          Tuple3 _ _ c  = array !! 0
 determinant m@(Matrix4 array) = a * a0 + b * a1 + c * a2 + d * a3
   where a0 = cofactor m 0 0
         a1 = cofactor m 0 1
         a2 = cofactor m 0 2
         a3 = cofactor m 0 3
         Tuple a _ _ _ = array !! 0
         Tuple _ b _ _ = array !! 0
         Tuple _ _ c _ = array !! 0
         Tuple _ _ _ d = array !! 0
 cofactor m i j | even (i + j) = minor m i j
                | otherwise = negate (minor m i j)
 minor m i j = determinant (submatrix m i j)
 transpose = matrix . map listToTuple . L.transpose . map getTupleList . row
 submatrix m i j = matrix (map listToTuple xs'')
    where  xs = map getTupleList (row m)
           xs' = L.transpose (take i xs ++ drop (i + 1) xs)
           xs'' = take j xs' ++ drop (j + 1) xs'

data Matrix a =  Matrix4 {
                         row :: [Tuple a]
                         }
               | Matrix3 {
                         row :: [Tuple a]
                         }
               | Matrix2 {
                         row :: [Tuple a]
                         }


instance Eq (Matrix Double) where
    a == b = all (==True) $ zipWith (==) (row a) (row b)

instance Num (Matrix Double) where
 a * b = matrix (map (\r -> (*|>) (transpose b) r) (row a))


instance Functor Matrix where
 fmap f (Matrix4 xs) = Matrix4 (fmap (fmap f) xs)
 fmap f (Matrix3 xs) = Matrix3 (fmap (fmap f) xs)
 fmap f (Matrix2 xs) = Matrix2 (fmap (fmap f) xs)

instance Applicative Matrix where
 pure a = Matrix4 (replicate 4 (pure a))
 (Matrix4 fs) <*> (Matrix4 as) = Matrix4 (zipWith (<*>) fs as)
 (Matrix3 fs) <*> (Matrix3 as) = Matrix3 (zipWith (<*>) fs as)
 (Matrix2 fs) <*> (Matrix2 as) = Matrix2 (zipWith (<*>) fs as)
instance Show a => Show (Matrix a) where
 show m = "Matrix" ++ d ++"x"++d ++ foldr (\a b ->'\n': show a++ b) "" (row m)
  where d = ((show . length . row) m)

matrix :: [Tuple a] -> Matrix a
matrix tuples@((Tuple2 _ _ ):_)    =    Matrix2 tuples
matrix tuples@((Tuple3 _ _ _):_)   =  Matrix3 tuples
matrix tuples@((Tuple  _ _ _ _):_) = Matrix4 tuples

fromListM :: [Double] -> Matrix Double
fromListM xs = matrix ((listToTuple x:fromListT d ys))
 where x  = take d xs
       ys = drop d xs
       d = (truncate . sqrt . fromIntegral . length) xs

fromListT :: Int -> [Double] -> [Tuple Double]
fromListT d xs = map listToTuple (chunkOf d xs)

listToTuple :: [Double] -> Tuple Double
listToTuple (x:y:z:w:_) = tuple (x,y,z,w)
listToTuple (x:y:z:_)   = tuple3 (x,y,z)
listToTuple (x:y:_)     = tuple2 (x,y)

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs
 | n > 0 = (take n xs) : (chunkOf n (drop n xs))
 | otherwise = error "Only positive numbers allowed"

get :: Matrix a -> (Int, Int) -> a
get m (i,j) = let tupl = (row m !! i) in case j of
    0 -> getX tupl
    1 -> getY tupl
    2 -> getZ tupl
    3 -> getW tupl

isPoint :: (Fractional a, Num a, Eq a) => Tuple a -> Bool
isPoint tuple = getW tuple == 1.0
isVector :: (Num a,Fractional a, Eq a) => Tuple a -> Bool
isVector tuple = getW tuple == 0.0
