module Plane where
import qualified Hitable
import Materials
import LinearAlgebra
import Rays
import Colors
import Transformations
data Plane = Plane {   getId :: !Int
                     , getPos :: !(Tuple Double)
                     , getTransform :: !(Matrix Double)
                     , getMaterial :: !Material}
 deriving Show

plane (id, pos,t, m) = Plane id pos t m

instance Hitable.Shape Plane where
  getId = getId
  getPos = getPos
  getPerimeter a = undefined
  getTransform = getTransform
  getMaterial = getMaterial
  localIntersect (a,r) = if abs ((getY . direction) r) < epsilon
                             then return []
                             else let t = (negate . getY . origin) r / (getY . direction) r
                                  in return [(Hitable.intersection (t,Hitable.Object a))]
  localNormalAt (a, p) = vector (0,1,0)

defaultPlane id = plane (id,point (0,0,0) ,scalingMatrix (10,0.01,10),m)
    where m = defaultMaterial