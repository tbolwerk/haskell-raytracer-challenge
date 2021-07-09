module SphereRaycasting where
import Tuples
import Canvas
import Colors
import Spheres
import Rays
rayOrigin :: Tuple Double
rayOrigin = point 0 0 (-5)
wallZ = 10
wallSize = 7
canvasPixels = 100
pixelSize :: Double
pixelSize = wallSize / canvasPixels
half = wallSize / 2

worldY :: Double -> Double
worldY y = half - pixelSize * y
worldX :: Double -> Double
worldX x = negate half + pixelSize * x
pos :: Double -> Double -> Tuple Double
pos x y = point (worldX x) (worldY y) wallZ
mr x y = ray (rayOrigin, (normalize ((pos x y) - rayOrigin)))

xs x y s= intersect (s, mr x y) 
{-
TODO: fix so it is looping inside a monad
-}
run = do
    let shape = sphere 1
    [ ((x,y), xs x y shape) | x <- [0.0..canvasPixels], y <- [0.0..canvasPixels]]
    
result :: [(Int, Int)]
result = map ((uncurry (\a b -> (round a, round b))). fst) (filter (\(_,xs) -> length xs /= 0) (map (\s -> (fst s, (eval (snd s) []))) run))

mapToCanvas = 
      let  c = canvas (round pixelSize) (round pixelSize)
           col = color 1 0 0 1 -- Should be red
           c2 = writePixels c result col
      in  createPPM c2 "chapter5.ppm"