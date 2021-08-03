module Transformations where
import           LinearAlgebra
type Scalar = Double
type Degree = Double
type Radians = Double


type From = Tuple Scalar
type To = Tuple Scalar
type Up = Tuple Scalar

viewTransformation :: (From,To,Up) -> Matrix Scalar
viewTransformation (from, to, up) = let forward = normalize (to - from)
                                        upn = normalize up
                                        left = cross forward upn
                                        trueUp = cross left forward
                                        orientation = listToMatrix 4 4 ( concat [
                                              [getX left, getY left, getZ left, 0]
                                            , [getX trueUp, getY trueUp, getZ trueUp, 0]
                                            , [negate (getX forward), negate (getY forward),  negate( getZ forward), 0]
                                            , [0,0,0,1]
                                         ])
                                      in orientation * translationMatrix (getX (negate from), getY (negate from), getZ (negate from))

shearing :: (Scalar, Scalar, Scalar, Scalar,Scalar,Scalar) -> (Tuple Scalar) -> (Tuple Scalar)
shearing (x1,x2,y0,y2,z0,z1) = matrixVectorMultiply (shearingMatrix (x1,x2,y0,y2,z0,z1))

shearingMatrix :: (Scalar, Scalar, Scalar,Scalar,Scalar,Scalar) -> Matrix Scalar
shearingMatrix (x1,x2,y0,y2,z0,z1) = listToMatrix 4 4 (concat [
    [1,x1,x2,0],
    [y0,1,y2,0],
    [z0,z1,1,0],
    [0,0,0,1]
                                                    ])

rotateZ :: Radians -> Tuple Scalar -> Tuple Scalar
rotateZ r = matrixVectorMultiply (rotateZMatrix r)

rotateZMatrix :: Radians -> Matrix Scalar
rotateZMatrix r = listToMatrix 4 4 (concat [
     [cos r, negate (sin r), 0, 0],
     [sin r, cos r, 0, 0],
     [0,0,1,0],
     [0,0,0,1]
                                 ])

rotateY :: Radians -> Tuple Scalar -> Tuple Scalar
rotateY r = matrixVectorMultiply (rotateYMatrix r)

rotateYMatrix :: Radians -> Matrix Scalar
rotateYMatrix r = listToMatrix 4 4 (concat [
     [cos r, 0, sin r, 0],
     [0, 1, 0, 0],
     [negate (sin r),0, cos r, 0],
     [0,0,0,1]
                                   ])

rotateX :: Radians -> Tuple Scalar -> Tuple Scalar
rotateX r = matrixVectorMultiply (rotateXMatrix r)

rotateXMatrix :: Radians -> Matrix Scalar
rotateXMatrix r = listToMatrix 4 4 (concat [
     [1,0,0,0],
     [0, cos r, negate (sin r), 0],
     [0,sin r, cos r, 0],
     [0,0,0,1]
                                   ])

radians :: Degree -> Radians
radians deg = (deg / 180) * pi

translation :: (Scalar, Scalar,Scalar) -> Tuple Scalar -> Tuple Scalar
translation (x,y,z) = matrixVectorMultiply (translationMatrix (x,y,z))

translationMatrix :: (Scalar, Scalar, Scalar) -> Matrix Scalar
translationMatrix (x,y,z) = listToMatrix 4 4 (concat [
    [1,0,0,x],
    [0,1,0,y],
    [0,0,1,z],
    [0,0,0,1]
                                            ])

scaling :: (Scalar, Scalar, Scalar) -> Tuple Scalar -> Tuple Scalar
scaling (x,y,z) = matrixVectorMultiply (scalingMatrix (x,y,z))
scalingMatrix :: (Scalar, Scalar, Scalar) -> Matrix Scalar
scalingMatrix (x,y,z) = listToMatrix 4 4 (concat [
    [x,0,0,0],
    [0,y,0,0],
    [0,0,z,0],
    [0,0,0,1]
                                       ])

-- transform :: Tuple Scalar -> Tuple Scalar
-- transform = translation (5,-3,2)

-- p :: Tuple Scalar
-- p = point 1 0 1

-- inv :: Matrix Scalar
-- inv = inverse transform

halfQuarterX :: Tuple Scalar -> Tuple Scalar
halfQuarterX = rotateX (pi / 4)
fullQuarterX :: Tuple Scalar -> Tuple Scalar
fullQuarterX = rotateX (pi / 2)
halfQuarterY :: Tuple Scalar -> Tuple Scalar
halfQuarterY = rotateY (pi / 4)
fullQuarterY :: Tuple Scalar -> Tuple Scalar
fullQuarterY = rotateY (pi / 2)

scalingP = scaling (5,5,5)

translateP = translation (10,5,7)

{-

use like:
p4 p
applies left to right.
returns: Tuple {getX = 15.0, getY = 0.0, getZ = 7.0, getW = 1.0}
-}

p4 = translateP . scalingP . fullQuarterX
