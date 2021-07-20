module HitableTest where
import           LinearAlgebra
import           Rays
import           Test.HUnit
import qualified Test.QuickCheck as T
import           Hitable
import Spheres
import World

testCase69 = assertEqual "should find 4 intersections" 4 (length $ intersectWorld (w, r2))
 where w = defaultWorld
testCase70 = assertEqual "the hit when an intersection occurs on the outside" False (inside check1)
testCase71 = assertEqual "the hit when an intersection occurs on the inside" True (inside check2)



r2 = ray (point (0,0,-5), vector (0,0,1))
s1 = defaultSphere 1
i1 = intersection (4, s1)
check1 = prepareComputation (i1,r2)

r3 = ray (point (0,0,0), vector (0,0,1))
s2 = defaultSphere 1
i2 = intersection (1, s2)
check2 = prepareComputation (i2,r3)

