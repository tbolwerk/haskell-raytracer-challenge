{-# LANGUAGE StrictData #-}
module Materials where
import           Colors
import           Pattern
import           Transformations
type Ambient = Double
type Diffuse = Double
type Specular = Double
type Shininess = Double
data Material = Material   {
                            color     :: !Color,
                            ambient   :: !Ambient,
                            diffuse   :: !Diffuse,
                            specular  :: !Specular,
                            shininess :: !Shininess,
                            pattern   :: !(Maybe Pattern)
                            }
                    deriving Show

material :: (Color,Ambient, Diffuse, Specular, Shininess, Maybe Pattern) -> Material
material (col,a, d, sp, sh,p) = Material col a d sp sh p


defaultMaterial = material (Colors.color 1 1 1 1, 0.1, 0.9,0.9,200.0, Nothing)
