module Chapter6 where
import Tuples
import Canvas
import Colors
import qualified Spheres
import Rays
import Transformations
import Control.Concurrent.Async
import Lights
import Control.Monad
import State
rayOrigin :: Tuple Double
rayOrigin = point 0 0 (-5)
wallZ = 10
wallSize = 7
canvasPixels :: Int
canvasPixels = 300
pixelSize :: Double
pixelSize = wallSize / (fromIntegral canvasPixels)
half = wallSize / 2

worldY :: Double -> Double
worldY y = half - pixelSize * y
worldX :: Double -> Double
worldX x = negate half + pixelSize * x
pos :: Double -> Double -> Tuple Double
pos x y = point (worldX x) (worldY y) wallZ


-- PART 2

light' = pointLight (point (-10) (10) (-10), color 1 1 1 1)

render :: Spheres.Sphere -> State [Spheres.Intersection] [Pixel]
render shape = foldM (\xs i -> foldM (\ys j -> do
    hit' <- (Spheres.intersect (shape, ray' i j))
    case hit' of
         Just hit'' -> let       point'' = Rays.position ((ray' i j), (Spheres.time) (hit''))  
                                 normal'' = Spheres.normalsAt (Spheres.object hit'', point'')
                                 eye'' = negate (direction (ray' i j)) 
                                 color' = lightning ((Spheres.getMaterial . Spheres.object) hit'', light', point'', eye'', normal'')
                     in return ((pixel (round i) (round j) color') : ys)
         Nothing -> return ys) xs (map fromIntegral [0..canvasPixels])) [] (map fromIntegral [0..canvasPixels])
 where ray' x y = ray (rayOrigin, (normalize ((pos x y) - rayOrigin)))

-- Fork four threads


main :: IO [()]
main = mapConcurrently execute [(shape, "chapter6.ppm"), (mShape, "chapter6_1.ppm"), (sShape,"chapter6_2.ppm"), (rShape,"chapter6_3.ppm")]
 where shape = Spheres.defaultSphere 1
       mShape = Spheres.setTransform' shape scalingMatrix (0.5, 0.5,0.5) 
       sShape = Spheres.setTransform shape (shearingMatrix (1,0,0,0,0,0) * scalingMatrix (0.5, 1, 1))
       rShape = Spheres.setTransform shape ((rotateZMatrix (radians 90)) * (scalingMatrix (1, 0.5,0.5)))


execute (shape, name)= (createPPM (eval (foldM (\c p -> return ((uncurry (writePixel c) (getPosition p)) (getColor p))) (canvas canvasPixels canvasPixels) (generateCanvas shape)) []) name)
 where generateCanvas s =  (eval (render s) [])