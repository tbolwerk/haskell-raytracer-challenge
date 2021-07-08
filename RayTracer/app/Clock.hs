module Clock where
{-
Part of the putting it all together of chapter 4 matrix transformations

drawing a clock using matrix transformations.
and write it to a ppm file
-}
import Transformations
import Matrices
import Tuples
import Canvas
import Colors

origin :: Tuple Scalar
origin = point 0 0 0

twelveOClock :: Tuple Scalar
twelveOClock =  (translation (0,0,1) origin)

clock :: [Tuple Scalar]
clock = map (translation (200, 200, 200) . scaling (150, 150,150)) [ rotateY (radians degree) twelveOClock | degree <- [0,30..360]]

mapClockToCanvas :: [Tuple Scalar] -> [(Int, Int)]
mapClockToCanvas tuples = map (\tup -> (round $ getX tup,  (round $ getZ tup))) tuples

main :: IO ()
main = do
    let c = canvas 400 400
        nc = writePixels c (mapClockToCanvas clock) (color 1 1 1 1)
    createPPM nc "clock.ppm"

