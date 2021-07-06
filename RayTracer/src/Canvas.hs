
module Canvas where
import Colors
import Tuples
import Control.Monad
import Data.Array

type Position = (Int, Int)

pixel :: Int -> Int -> Color -> Pixel
pixel x y col = Pixel (x,y) col

data Pixel = Pixel { getPosition :: Position , getColor :: Color }
 deriving Show

--- State monad
--- s = Pixels
--- a = Canvas

data Canvas a = Canvas { getWidth :: a, getHeight :: a, pixels :: Array (Int,Int) Pixel } 
 deriving Show

canvas :: Int -> Int -> Canvas Int 
canvas width height = Canvas width height (pixelArray ((0,0), (width,height)) [ pixel x y (color 0 0 0 1) | x <- [0..width], y <- [0..height] ])

writePixel :: Canvas Int -> Int -> Int -> Color -> Canvas Int
writePixel c x y col = Canvas (getWidth c) (getHeight c) ((pixels c) // [((x,y), pixel x y col)])

pixelAt :: Canvas Int -> Int -> Int -> Pixel
pixelAt c x y = (!) (pixels c) (x,y) 

pixelArray :: ((Int, Int), (Int, Int)) -> [Pixel] -> Array (Int,Int) Pixel
pixelArray bounds pixels = array bounds [(getPosition p, p) | p <- pixels]