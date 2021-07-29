{-# LANGUAGE Strict #-}
module Chapter6 where
import           Canvas
import           Colors
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Array               as A
import           Hitable
import           Lights
import           LinearAlgebra
import           Materials
import           Rays
import qualified Spheres
import           State
import           Transformations
rayOrigin :: Tuple Double
rayOrigin = point (0, 0, (-5))
wallZ = 10
wallSize = 7
canvasPixels :: Int
canvasPixels = 100
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

light' = pointLight (point ((-10), (10), (-10)), Colors.color 1 1 1 1)

render :: Object -> State [Hitable.Intersection] [Pixel]
render shape = foldM (\xs i -> foldM (\ys j -> do
    hit' <- (Hitable.intersect (shape, ray' i j))
    case hit' of
          (hit'':_) -> let       point'' = Rays.position ((ray' i j), (Hitable.time) (hit''))
                                 normal'' = Hitable.normalsAt (Hitable.object hit'', point'')
                                 eye'' = negate (direction (ray' i j))
                                 color' = lighting ((Hitable.getMaterial . Hitable.object) hit'', light', point'', eye'', normal'',False)
                     in return ((pixel (round i) (round j) color') : ys)
          [] -> return ((pixel (round i) (round j) (Colors.color 0 0 0 1)) : ys)) xs (map fromIntegral [(canvasPixels-1),(canvasPixels-2)..0])) [] (map fromIntegral [(canvasPixels-1),(canvasPixels-2)..0])
 where
     ray' :: Double -> Double -> Ray
     ray' x y = ray (rayOrigin, (normalize ((pos x y) - rayOrigin)))


-- Fork four threads

materialSphere = material (Colors.color 1 0.2 1 1, 0.1, 0.9, 0.9, 200.0)


main :: IO [()]
main = mapConcurrently execute [(Object shape, "chapter6.ppm"), (Object mShape, "chapter6_1.ppm"), (Object sShape,"chapter6_2.ppm"), (Object rShape,"chapter6_3.ppm")]
 where shape = Spheres.setMaterial (Spheres.defaultSphere 1) (materialSphere)
       mShape = Spheres.setTransform' shape scalingMatrix (0.5, 0.5,0.5)
       sShape = Spheres.setTransform shape (shearingMatrix (1,0,0,0,0,0) * scalingMatrix (0.5, 1, 1))
       rShape = Spheres.setTransform shape ((rotateZMatrix (radians 90)) * (scalingMatrix (1, 0.5,0.5)))

execute ::  (Object, String) -> IO ()
execute (shape, name)= (createPPM (generateCanvas shape) name)
 where generateCanvas s =  Canvas (canvasPixels-1) (canvasPixels -1) (A.listArray ((0,0), (canvasPixels-1,canvasPixels-1)) (eval (render s) []))


