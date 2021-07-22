{-# LANGUAGE StrictData #-}
module Materials where
import           Colors
type Ambient = Double
type Diffuse = Double
type Specular = Double
type Shininess = Double
data Material = Material   {
                            color     :: !Color,
                            ambient   :: !Ambient,
                            diffuse   :: !Diffuse,
                            specular  :: !Specular,
                            shininess :: !Shininess
                            }
                    deriving Show

material :: (Color,Ambient, Diffuse, Specular, Shininess) -> Material
material (col,a, d, sp, sh) = Material col a d sp sh


defaultMaterial = material (Colors.color 1 1 1 1, 1, 0.9,0.9,200.0)
