module HitableTest where
import           Colors
import           Hitable
import           LinearAlgebra
import           Rays
import           Spheres
import           Test.HUnit
import qualified Test.QuickCheck as T
import           World

testCase69 = assertEqual "should find 4 intersections" 4 (length $ intersectWorld (w, r2))
 where w = defaultWorld
testCase70 = assertEqual "the hit when an intersection occurs on the outside" False (inside check1)
testCase71 = assertEqual "the hit when an intersection occurs on the inside" True (inside check2)
testCase72 = assertEqual "equals color" (Colors.color 0 0 0 1) (colorAt (w1,r1))
-- testCase73 = assertEqual "equals color" (Colors.color 0.38066 0.47583 0.2855 0) (colorAt (w1,r2))

r2 = ray (point (0,0,-5), vector (0,0,1))
s1 = defaultSphere 1
i1 = intersection (4, Object s1)
check1 = prepareComputation (i1,r2)

r3 = ray (point (0,0,0), vector (0,0,1))
s2 = defaultSphere 1
i2 = intersection (1, Object s2)
check2 = prepareComputation (i2,r3)


w1 = defaultWorld
r1 = ray (point (0,0,5), vector (0,1,0))
-- r2 = ray (point (0,0,-5), vector (0,0,1))




r4 = ray (point (0,0,0.75),vector (0,0,-1))





