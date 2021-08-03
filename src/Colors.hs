module Colors where
import           LinearAlgebra
type Red = Double
type Green = Double
type Blue = Double
type Alpha = Double

type Color = Tuple Double

color :: Red -> Green -> Blue -> Alpha -> Color
color r g b a = tuple (r, g, b, a)

black :: Color
black = color 0 0 0 1

white :: Color
white = color 1 1 1 1

red :: Color
red = color 1 0 0 1
green :: Color
green = color 0 1 0 1
blue :: Color
blue = color 0 0 1 1

getRed :: Color -> Double
getRed c = getX c
getGreen :: Color -> Double
getGreen c = getY c
getBlue :: Color -> Double
getBlue c = getZ c
getAlpha :: Color -> Double
getAlpha c = getW c

hadamardProduct :: Color -> Color -> Color
hadamardProduct c1 c2 =
    let r = getRed c1 * getRed c2
        g = getGreen c1 * getGreen c2
        b = getBlue c1 * getBlue c2
        a = getAlpha c1 * getAlpha c2
     in color r g b a
