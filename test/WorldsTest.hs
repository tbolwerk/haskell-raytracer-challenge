module WorldsTest where

import           Worlds
import LinearAlgebra
import Rays
import           Test.HUnit
import qualified Test.QuickCheck as T

r2 ::  Ray
r2  = ray (rayOrigin, vector (0,0,1))

rayOrigin :: Tuple Double
rayOrigin = point (0, 0, (-5))


testCase69 = assertEqual "should find 4 intersections" 4 (length $ intersectWorld (w, r2))
 where w = defaultWorld