module CanvasTest where
import           Canvas
import           Colors
import           Data.Array
import           Test.HUnit
import qualified Test.QuickCheck as T
type CanvasInput = (Int, Int)

testCase32 = assertBool "every pixel of canvas is black" (all (\x -> Canvas.getColor x == (color 0 0 0 1)) (elems (Canvas.pixels (Canvas.canvas 10 20))))
testCase33 = assertBool "c.width = 10" ((Canvas.getWidth (Canvas.canvas 10 20)) == (10 - 1))
testCase34 = assertBool "c.height = 20" ((Canvas.getHeight (Canvas.canvas 10 20)) == (20 - 1))
testCase35 = assertBool "pixel at (2,3) = red" (let c = Canvas.canvas 10 20
                                                    red = color 1 0 0 1
                                                    nc = Canvas.writePixel c 2 3 red
                                               in (Canvas.getColor (Canvas.pixelAt nc 2 3)) == red)


prop_Canvas_Create :: CanvasInput -> Bool
prop_Canvas_Create (width, height) = ((getWidth c == width -1) &&
                                     (getHeight c == height -1) &&
                                     predicate)
  where c = canvas width height
        isBlack col = getRed col == 0 && getGreen col == 0 && getBlue col == 0
        predicate = all (\x -> isBlack (getColor x)) (pixels c)

prop_Canvas_PixelAt :: CanvasInput -> Bool
prop_Canvas_PixelAt (width, height) = (width <= 0 || height <= 0) || isBlack (getColor (pixelAt c 0 0))
    where c = canvas width height
          isBlack col = getRed col == 0 && getGreen col == 0 && getBlue col == 0

prop_Canvas_WritePixel :: CanvasInput -> Bool
prop_Canvas_WritePixel (width, height) = (width <= 0 || height <= 0) || (black /= actualColor) && (actualColor == red)
    where c = canvas width height
          nc = writePixel c 0 0 red
          red = color 1 0 0 0
          black = color 0 0 0 0
          actualColor = getColor (pixelAt nc 0 0)

prop_Canvas_PPM_EndsWithNewLine :: CanvasInput -> Bool
prop_Canvas_PPM_EndsWithNewLine (width, height) = (width <= 0 || height <= 0) || predicate
 where c = canvas width height
       predicate = last (canvasToString c) == '\n'


