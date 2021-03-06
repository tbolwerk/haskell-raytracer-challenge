module Lights where
import           Colors
import           Hitable
import           LinearAlgebra
import           Materials
data Light = PointLight {
                               position    :: !(Tuple Double)
                               , intensity :: !(Tuple Double)
                        }
 deriving Show

type EyeVector = Tuple Double
type NormalVector = Tuple Double

pointLight :: (Tuple Double, Tuple Double) -> Light
pointLight (p, i) = PointLight p i

{-
lighting (defaultMaterial, pl, point 0 0 0, vector 0 0 (-1), vector 0 0 (-1))
Tuple {getX = 0.1, getY = 0.1, getZ = 0.1, getW = 0.1}
-}

lighting :: (Material, Object,Light, Tuple Double, EyeVector, NormalVector, Bool) -> Color
lighting (m, o,light, p, ev, nv, inShadow) = if lightDotNormal < 0 || inShadow
                                          then ambient' + black + black
                                          else ambient' + diffuse' + specular'
                            where col = case Materials.pattern m of
                                               Just x  -> patternAtShape x o p
                                               Nothing -> Materials.color m
                                  effectiveColor = col * intensity light
                                  lightVector = normalize (position light - p)
                                  ambient' = effectiveColor * pure (ambient m)
                                  lightDotNormal = dot lightVector nv
                                  diffuse' = effectiveColor * pure (diffuse m) * pure lightDotNormal
                                  reflectVector = reflect (negate lightVector, nv)
                                  reflectDotEye = dot reflectVector ev
                                  specular' = if reflectDotEye <= 0
                                                    then black
                                                    else intensity light * pure (specular m) * pure (reflectDotEye ** shininess m)


pl = pointLight (point (0, 0, 10), Colors.color 1 1 1 1)


