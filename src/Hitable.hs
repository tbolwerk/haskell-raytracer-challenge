{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}
module Hitable where

import qualified Data.List     as List
import           LinearAlgebra
import           Materials
import           Rays
import qualified Spheres       as S
import           State
import Colors
import qualified Pattern as Pattern
data Computation  = Computation
    { computationTime   :: !Double
    , computationObject :: !Object
    , computationPoint  :: !(Tuple Double)
    , computationEye    :: !(Tuple Double)
    , computationNormal :: !(Tuple Double)
    , inside            :: !Bool
    , computationOverPoint :: !(Tuple Double)
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
      , computationOverPoint = cPoint + cNormal' * pure epsilon
} where
        cTime = time i
        cPoint = Rays.position (r,cTime)
        cObject = object i
        cNormal = normalsAt (cObject, cPoint)
        cEye = negate (direction r)

class Shape a where
  getId :: a -> Int
  getPos :: a -> Tuple Double
  getPerimeter :: a -> Double
  getTransform :: a -> Matrix Double
  getMaterial :: a -> Material
  localIntersect :: (a,Ray) -> State [Intersection] [Intersection]
  localNormalAt :: (a, Tuple Double) -> Tuple Double

instance Shape S.Sphere where
  getId :: S.Sphere -> Int
  {-# INLINE getId #-}
  getId = S.getId
  getPos :: S.Sphere -> Tuple Double
  {-# INLINE getPos #-}
  getPos = S.getPos
  getPerimeter :: S.Sphere -> Double
  {-# INLINE getPerimeter #-}
  getPerimeter = S.getR
  getTransform :: S.Sphere -> Matrix Double
  {-# INLINE getTransform #-}
  getTransform = S.getTransform
  getMaterial :: S.Sphere -> Material
  {-# INLINE getMaterial #-}
  getMaterial = S.getMaterial
  localIntersect :: (S.Sphere, Ray) -> State [Intersection] [Intersection]
  {-# INLINE localIntersect #-}
  localIntersect (s, r) = let d = (discriminant a b c)
     in if d < 0
          then return []
          else return (hit (quadraticEquation d))
    where
      sphereToRay :: Tuple Double
      sphereToRay = origin r - getPos s
      a :: Double
      a = dot (direction r) (direction r)
      b :: Double
      b = 2 * dot (direction r) sphereToRay
      c :: Double
      c = dot sphereToRay sphereToRay - 1
      quadraticEquation :: Double -> [Intersection]
      quadraticEquation d =
        map (\x -> intersection ((x / (2 * a)), (Object s))) ((negate b) ± (sqrt d))
  localNormalAt :: (S.Sphere, Tuple Double) -> Tuple Double
  localNormalAt (s, p) = worldNormal
                           where
                            Tuple x y z _ = p - getPos s --TODO: World point
                            worldNormal = vector (x, y, z)

class Hitable a where
  intersect :: (a, Ray) -> State [Intersection] [Intersection]
  normalsAt :: (a, Tuple Double) -> Tuple Double


-- instance Hitable S.Sphere where
--   intersect :: (S.Sphere, Ray) -> State [Intersection] [Intersection]
--   {-# INLINE intersect #-}
--   intersect (s, r') =
--     let d = (discriminant a b c)
--      in if d < 0
--           then return []
--           else return (hit (quadraticEquation d))
--     where
--       r :: Ray
--       r = transform ((inverse . getTransform) s) r'
--       sphereToRay :: Tuple Double
--       sphereToRay = origin r - (getPos s)
--       a :: Double
--       a = dot (direction r) (direction r)
--       b :: Double
--       b = 2 * dot (direction r) sphereToRay
--       c :: Double
--       c = dot sphereToRay sphereToRay - 1
--       quadraticEquation :: Double -> [Intersection]
--       quadraticEquation d =
--         map (\x -> intersection (x / (2 * a), s)) ((negate b) ± (sqrt d))

hit :: [Intersection] -> [Intersection]
{-# INLINE hit #-}
hit xs =  ((List.sort . filter (\x -> time x >= 0)) xs)

headOr :: [a] -> Maybe a
headOr []    = Nothing
headOr (x:_) = Just x

discriminant :: Double -> Double -> Double -> Double
{-# INLINE discriminant #-}
discriminant a b c = b ^ 2 - 4 * a * c

infixl 6 ±

(±) :: Num a => a -> a -> [a]
-- ascending order, recommended by book
(±) a b = [a - b, a + b]


data Object = forall a. (Shape a, Show a) => Object !a

instance Shape Object where
  getId (Object a) = getId a
  getMaterial (Object a) = getMaterial a
  getPos (Object a) = getPos a
  getPerimeter (Object a) = getPerimeter a
  getTransform (Object a) = getTransform a
  localIntersect (Object a,r) = localIntersect (a,r)
  localNormalAt (Object a, p) = localNormalAt (a,p)

instance Hitable Object where
  intersect ((Object a), r) = let localRay = transform ((inverse . getTransform) a) r
                              in localIntersect (Object a, localRay)
  normalsAt ((Object a), p) = let localPoint  = matrixVectorMultiply ((inverse . getTransform) a) p
                                  localNormal = localNormalAt (a, localPoint)
                                  (Tuple x y z _) = matrixVectorMultiply (transpose (inverse (getTransform a))) localNormal
                                  worldNormal = vector (x,y,z)
                              in normalize worldNormal



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
intersection ::  (Shape a,Hitable a, Show a) => (Time, a) -> Intersection
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


patternAtShape :: Pattern.Pattern -> Object -> Tuple Double -> Color
patternAtShape p o wp= let objectPoint = matrixVectorMultiply ((inverse . getTransform) o) wp
                           patternPoint = matrixVectorMultiply ((inverse . Pattern.getTransform) p) objectPoint
                       in Pattern.patternAt p patternPoint