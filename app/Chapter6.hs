{-# LANGUAGE Strict #-}
module Chapter6 where
import           Canvas
import           Colors
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Array               as A
import           Lights
import           LinearAlgebra
import           Rays
import qualified Spheres
import           State
import           Transformations
import Shape 

rayOrigin :: Tuple Double
rayOrigin = point (0, 0, (-5))
wallZ = 10
wallSize = 7
canvasPixels :: Int
canvasPixels = 500
pixelSize :: Double
pixelSize = wallSize / (fromIntegral canvasPixels)
half = wallSize / 2

worldY :: Double -> Double
worldY y = half - pixelSize * y
worldX :: Double -> Double
worldX x = negate half + pixelSize * x
pos :: Double -> Double -> Tuple Double
pos x y = point ((worldX x), (worldY y), wallZ)


-- PART 2

light' = pointLight (point ((-10), (10), (-10)), color 1 1 1 1)

render :: Shape.Shape -> State [Shape.Intersection] [Pixel]
render shape = foldM (\xs i -> foldM (\ys j -> do
    hit' <- (Shape.intersect (shape, ray' i j))
    case hit' of
         (hit'':_) -> let        point'' = Rays.position ((ray' i j), (Shape.time) (hit''))
                                 normal'' = Spheres.normalsAt (Shape.object hit'', point'')
                                 eye'' = negate (direction (ray' i j))
                                 color' = lightning ((Shape.material . Shape.object) hit'', light', point'', eye'', normal'')
                     in return ((pixel (round i) (round j) color') : ys)
         [] -> return ((pixel (round i) (round j) (color 0 0 0 1)) : ys)) xs (map fromIntegral [(canvasPixels-1),(canvasPixels-2)..0])) [] (map fromIntegral [(canvasPixels-1),(canvasPixels-2)..0])
 where
     ray' :: Double -> Double -> Ray
     ray' x y = ray (rayOrigin, (normalize ((pos x y) - rayOrigin)))

-- Fork four threads


main :: IO [()]
main = mapConcurrently execute [(shape, "chapter6.ppm"), (mShape, "chapter6_1.ppm"), (sShape,"chapter6_2.ppm"), (rShape,"chapter6_3.ppm")]
 where shape = Spheres.defaultSphere 1
       mShape = Spheres.setTransform' shape scalingMatrix (0.5, 0.5,0.5)
       sShape = Spheres.setTransform shape (shearingMatrix (1,0,0,0,0,0) * scalingMatrix (0.5, 1, 1))
       rShape = Spheres.setTransform shape ((rotateZMatrix (radians 90)) * (scalingMatrix (1, 0.5,0.5)))

execute :: (Shape.Shape, String) -> IO ()
execute (shape, name)= (createPPM (generateCanvas shape) name)
 where generateCanvas s =  Canvas (canvasPixels-1) (canvasPixels -1) (A.listArray ((0,0), (canvasPixels-1,canvasPixels-1)) (eval (render s) []))


