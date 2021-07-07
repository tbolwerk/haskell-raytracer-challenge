{-# LANGUAGE OverloadedStrings #-}
module Canvas where
import Colors
import Tuples
import Control.Monad
import Data.Array
import System.IO

type Position = (Int, Int)

pixel :: Int -> Int -> Color -> Pixel
pixel x y col = Pixel (x,y) col

data Pixel = Pixel { getPosition :: Position , getColor :: Color }
 deriving Show

-- Probably should be a monad for canvas
--- State monad
--- s = Pixels
--- a = Canvas

data Canvas a = Canvas { getWidth :: a, getHeight :: a, pixels :: Array (Int,Int) Pixel } 
 deriving Show

canvas :: Int -> Int -> Canvas Int 
canvas width height = Canvas w h (pixelArray ((0,0), (w,h)) [ pixel x y (color 0 0 0 1) | x <- [0..w], y <- [0..h] ])
 where w = width - 1
       h = height - 1

writePixel :: Canvas Int -> Int -> Int -> Color -> Canvas Int
writePixel c x y col = Canvas (getWidth c) (getHeight c) ((pixels c) // [((x,y), pixel x y col)])

filterOutOfBound :: ((Int, Int), (Int, Int)) -> [(Int, Int)] -> [(Int, Int)]
filterOutOfBound ((lx, ly), (hx, hy)) cords = filter (\cord -> isOutOfBound ((lx, ly), (hx, hy)) cord) cords

type Bounds = ((Int, Int), (Int, Int))

isOutOfBound :: Bounds -> (Int, Int) -> Bool
isOutOfBound bounds (x,y) = x >= lx && y >= ly && x <= hx && y <= hy
    where ((lx, ly), (hx, hy)) = bounds 

writePixels :: Canvas Int -> [(Int, Int)] -> Color -> Canvas Int
writePixels c cords col = Canvas (getWidth c) (getHeight c) ((pixels c) // (map (\cord@(x,y) -> (cord, pixel x y col))) (filterOutOfBound ((0,0), (getWidth c, getHeight c)) cords))
 where pxs = map (\(x,y) -> pixel x y (color 1 1 1 1)) cords


filterOutOfBound' :: Array (Int, Int) Pixel -> [((Int, Int),Pixel)]
filterOutOfBound' array = filter (\(cord,_) -> isOutOfBound (bounds array) cord) (assocs array)

writePixels' :: Canvas Int -> Array (Int, Int) Pixel -> Canvas Int
writePixels' c ps = Canvas (getWidth c) (getHeight c) ((pixels c) // (filterOutOfBound' ps))

pixelAtCord :: Array (Int, Int) Pixel -> (Int , Int) -> Pixel
pixelAtCord pixels (x,y) = (!) pixels (x,y) 


pixelAt :: Canvas Int -> Int -> Int -> Pixel
pixelAt c x y  = (!) (pixels c) (x,y) 

pixelArray :: ((Int, Int), (Int, Int)) -> [Pixel] -> Array (Int,Int) Pixel
pixelArray bounds pixels = array bounds [(getPosition p, p) | p <- pixels]

createPPM :: Canvas Int -> FilePath -> IO ()
createPPM c path = writeFile path content
 where content = canvasToString c

canvasToString :: Canvas Int -> String
canvasToString c = ("P3\n" ++ ((show (getWidth c + 1)) ++ " " ++ (show (getHeight c + 1))) ++ "\n" ++ "255\n" ++ pixelData c ++ "\n")

colorDepth = 255

pixelData :: Canvas Int -> String
pixelData c = format (foldMap (\x -> (show (rgbCode x))) (horizontalIter p))
 where p = pixels c
       rgbCode pixel = show ((clamp . getRed) col) ++ space ++ show ((clamp . getGreen) col) ++ space ++ show ((clamp . getBlue) col)
         where col = getColor pixel

myCanvas = canvas 5 5

newCanvas = writePixels myCanvas [(0, 0), (0, 3), (3,0), (3,3)] (color 1 0 1 1)

mpixels = pixels newCanvas

horizontalIter :: Array (Int, Int) Pixel -> [Pixel]
horizontalIter array = [ pixelAtCord array (x, y) | y <- [ly.. hy], x <- [lx .. hx]]
 where ((lx, ly), (hx, hy)) = bounds array


clamp :: Double -> Int
clamp double | double <= 0 = 0
             | double >= 1 = 255
             | otherwise = round (double * 255)

space = " "

replace :: String -> String
replace ('"':'"':xs) = "\n" ++ replace xs
replace (x:xs) = x : replace xs
replace "" = ""

format :: String -> String
format input = filter (/= '"') doubleQuoted
 where doubleQuoted = replace input


