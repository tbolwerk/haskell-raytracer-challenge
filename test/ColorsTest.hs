module ColorsTest where
import Colors
import qualified Test.QuickCheck as T
import Test.HUnit
testCase25 = assertEqual "c.red = -0.5" (-0.5) (getRed (color (-0.5) 0.4 1.7 1))
testCase26 = assertEqual "c.green = 0.4" 0.4 (getGreen (color (-0.5) 0.4 1.7 1))
testCase27 = assertEqual "c.blue = 1.7" 1.7 (getBlue (color (-0.5) 0.4 1.7 1))
testCase28 = assertEqual "c1 + c2" (color 1.6 0.7 1.0 1.0) (color 0.9 0.6 0.75 1 + color 0.7 0.1 0.25 0)
testCase29 = assertEqual "c1 - c2" (color 0.2 0.5 0.5 1.0) (color 0.9 0.6 0.75 1 - color 0.7 0.1 0.25 0)
testCase30 = assertEqual "c1 * 2" (color 0.4 0.6 0.8 2.0) (color 0.2 0.3 0.4 1 * pure 2)
testCase31 = assertEqual "c1 * c2" (color 0.9 0.2 0.04 1) (color 1 0.2 0.4 1.0 * color 0.9 1 0.1 1)