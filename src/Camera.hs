
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Camera where
import qualified Rays as Rays
import LinearAlgebra
import Canvas
import Control.Monad
import State
import World
data Camera = Camera { hSize :: Int
                     , vSize :: Int
                     , fov :: Double
                     , transform :: Matrix Double
                     , halfWidth :: Double
                     , halfHeight :: Double
                     , pixelSize :: Double
                     }
 deriving Show

setViewTransform :: Camera -> Matrix Double -> Camera
setViewTransform c vt = Camera (hSize c) (vSize c) (fov c) vt (halfWidth c) (halfHeight c) (pixelSize c)

camera :: (Int, Int, Double) -> Camera
camera (hsize, vsize, fov) = Camera hsize vsize fov identityMatrix halfWidth halfHeight pixelSize
    where
     halfView = tan (fov / 2)
     aspect =  ((fromIntegral hsize) / (fromIntegral vsize))
     (halfWidth, halfHeight) = if aspect >= 1 then
                                 (halfView, halfView / aspect)
                               else
                                 (halfView * aspect, halfView)
     pixelSize = (halfWidth * 2) / (fromIntegral hsize)
     
rayForPixel :: (Camera, Int, Int) -> Rays.Ray
rayForPixel (camera, px , py) = Rays.ray (origin', direction')
    where
        xOffset = (fromIntegral px + 0.5) * pixelSize camera
        yOffset = (fromIntegral py + 0.5) * pixelSize camera
        worldX = halfWidth camera - xOffset
        worldY = halfHeight camera - yOffset
        pixel' = inverse (transform camera) *|> point (worldX, worldY, negate 1)
        origin' = inverse (transform camera) *|> point (0,0,0)
        direction' = normalize (pixel' - origin')

render :: (Camera, World) -> State [Pixel] [Pixel]
render (camera, world) = foldM (\ys y -> foldM (\xs x -> 
                                 let ray' = rayForPixel (camera, x, y)
                                     color' = colorAt (world, ray')
                                  in return ((pixel x y color') : xs)) ys [width,width-1..0]) [] [height,height-1..0]
  where width = (hSize camera) - 1
        height = (vSize camera) -1

                        





