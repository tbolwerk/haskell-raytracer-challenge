module SphereRaycasting where
import Tuples
import Canvas
import Colors
import Spheres
import Rays
import Transformations
import Control.Concurrent
import Lights
import Control.Monad
import State
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
TODO: fix so it is looping inside the state monad

change shape to get a different image
-}
run shape = 
    [ ((x,y), xs x y shape) | x <- [0.0..canvasPixels], y <- [0.0..canvasPixels]]
    
result :: Sphere -> [(Int, Int)]
result shape = map (uncurry (\a b -> (round a, round b)). fst) (filter (\(_,xs) -> length xs /= 0) (map (\s -> (fst s, (eval (snd s) []))) (run shape)))




mapToCanvas = do
    let shape = defaultSphere 1
        mShape = setTransform' shape scalingMatrix (0.5, 0.5,0.5) 
        sShape = setTransform shape (shearingMatrix (1,0,0,0,0,0) * scalingMatrix (0.5, 1, 1))
        rShape = setTransform shape ((rotateZMatrix (radians 90)) * (scalingMatrix (1, 0.5,0.5)))
    forkIO (createPPM (writePixels (canvas 100 100) (result shape) (color 1 0 0 1) ) "chapter5.ppm")
    forkIO (createPPM (writePixels (canvas 100 100) (result mShape) (color 1 0 0 1) ) "chapter5_1.ppm")
    forkIO (createPPM (writePixels (canvas 100 100) (result sShape) (color 1 0 0 1) ) "chapter5_2.ppm")
    forkIO (createPPM (writePixels (canvas 100 100) (result rShape) (color 1 0 0 1) ) "chapter5_3.ppm")


-- PART 2

light' = pointLight (point (-10) (10) (-10), color 1 1 1 1)
point' ray' hit' = Rays.position (ray', ( time) hit') 
normal' point' hit' = normalsAt (object hit', point')
eye' ray'= negate (direction ray')

calcColor (hit', light', point', eye', normal') = lightning ((Spheres.getMaterial . object) hit', light', point', eye', normal')

run' :: Sphere -> State [Intersection] [[([Pixel], Maybe [Intersection])]]
run' shape = (forM [0..100] (\i -> forM [0..100] (\j -> do
       hit' <- (intersect (shape, ray' i j))
       case hit' of
           Just a -> let     hit'' = head a
                             point'' = point' (ray' i j) (hit'')
                             normal'' = normal' point'' hit''
                             eye'' = eye' (ray' i j)
                             color' = lightning ((getMaterial . object) hit'', light', point'', eye'', normal'')
                     in return ([pixel (round i) (round j) color'], hit')
           Nothing -> return ([], Nothing) 

      
    )))
     where  ray' x y = ray (rayOrigin, (normalize ((pos x y) - rayOrigin)))



mapToCanvas' :: IO ()
mapToCanvas' = createPPM (eval (foldM (\c p -> return (writePixel c ((fst . getPosition) p) ((snd . getPosition) p) (getColor p))) (canvas 100 100) xs) []) "chapter6.ppm"
 where xs = (concat . concat) (map (map fst) (eval (run' s) []))
