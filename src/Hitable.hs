{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
module Hitable where

import qualified Data.List     as List
import           LinearAlgebra
import           Materials
import           Rays
import qualified Spheres       as S
import           State

data 
     Computation  =
  Computation
    { computationTime   :: Double
    , computationObject :: Object
    , computationPoint  :: Tuple Double
    , computationEye    :: Tuple Double
    , computationNormal :: Tuple Double
    , inside :: Bool
    }
  deriving (Show)

prepareComputation :: (Intersection, Ray) -> Computation
prepareComputation (i,r) = 
    let (isInside, cNormal') = if dot cNormal cEye < 0 
                               then (True, negate cNormal)
                               else (False,cNormal)
    in Computation {
        computationTime = cTime
      , computationObject = cObject
      , computationPoint = cPoint
      , computationEye = cEye
      , computationNormal = cNormal'
      , inside = isInside
} where
        cTime = time i
        cPoint = Rays.position (r,cTime)
        cObject = object i
        cNormal = normalsAt (cObject, cPoint)
        cEye = negate (direction r)
         
        
         

class Hitable a where
  getId :: a -> Int
  getPos :: a -> Tuple Double
  getR :: a -> Double
  getTransform :: a -> Matrix Double
  getMaterial :: a -> Material
  intersect :: (a, Ray) -> State [Intersection] [Intersection]



instance Hitable S.Sphere where
  getId :: S.Sphere -> Int
  getId = S.getId
  {-# INLINE getId #-}
  getPos :: S.Sphere -> Tuple Double
  getPos = S.getPos
  {-# INLINE getPos #-}
  getR :: S.Sphere -> Double
  getR = S.getR
  {-# INLINE getR #-}
  getTransform :: S.Sphere -> Matrix Double
  getTransform = S.getTransform
  {-# INLINE getTransform #-}
  getMaterial :: S.Sphere -> Material
  getMaterial = S.getMaterial
  {-# INLINE getMaterial #-}
  intersect :: (S.Sphere, Ray) -> State [Intersection] [Intersection]
  {-# INLINE intersect #-}
  intersect (s, r') =
    let d = (discriminant a b c)
     in if d < 0
          then return []
          else return (hit (quadraticEquation d))
    where
      r :: Ray
      r = transform ((inverse . getTransform) s) r'
      sphereToRay :: Tuple Double
      sphereToRay = origin r - (getPos s)
      a :: Double
      a = dot (direction r) (direction r)
      b :: Double
      b = 2 * dot (direction r) sphereToRay
      c :: Double
      c = dot sphereToRay sphereToRay - 1
      quadraticEquation :: Double -> [Intersection]
      quadraticEquation d =
        map (\x -> intersection (x / (2 * a), s)) ((negate b) ± (sqrt d))

hit :: [Intersection] -> [Intersection]
{-# INLINE hit #-}
hit xs =  ((List.sort . filter (\x -> time x >= 0)) xs)


headOr :: [a] -> Maybe a
headOr []    = Nothing
headOr (x:_) = Just x

normalsAt :: (Hitable a, Show a) => (a, Tuple Double) -> Tuple Double
{-# INLINE normalsAt #-}
normalsAt (s, p) = normalize worldNormal
  where
    objectPoint = matrixVectorMultiply (inverse (getTransform s)) p
    objectNormal = objectPoint - getPos s --TODO: World point
    Tuple x y z _ =
      matrixVectorMultiply (transpose (inverse (getTransform s))) objectNormal
    worldNormal = vector (x, y, z)
discriminant :: Double -> Double -> Double -> Double
{-# INLINE discriminant #-}
discriminant a b c = b ^ 2 - 4 * a * c

infixl 6 ±

(±) :: Num a => a -> a -> [a]
-- ascending order, recommended by book
(±) a b = [a - b, a + b]

data Object = forall a. (Hitable a, Show a) => Object !a

instance Hitable Object where
  getId (Object a) = getId a
  getMaterial (Object a) = getMaterial a
  getPos (Object a) = getPos a
  getR (Object a) = getR a
  getTransform (Object a) = getTransform a
  intersect ((Object a), r) = intersect (a, r)

instance Show Object where
  show (Object a) = show a

instance Eq Intersection where
  (==) a b = time a == time b && (getId . object) a == (getId . object) b

-- We are looking for the lowest non-negative value of t (time)
instance Ord Intersection where
  (<=) a b = time a <= time b && time a >= 0


{-
Difference between 60 seconds run and 260 seconds run
custom object vs predefined Sphere object in intersection object.

-}
intersection ::  (Hitable a, Show a) => (Time, a) -> Intersection
intersection (t, a) = Intersection t (Object a)

data Intersection =
  Intersection
    { time   :: !Time
    , object :: !Object
    }
  deriving (Show)

{-
this code performs 4x faster.


intersection ::   (Time, S.Sphere) -> Intersection
intersection (t, a) = Intersection t a


data Intersection =
  Intersection
    { time   :: !Time
    , object :: !S.Sphere
    }
  deriving (Show)
-}

