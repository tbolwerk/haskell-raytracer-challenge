module Pattern where
import Colors
import LinearAlgebra

data Pattern = StripePattern { a :: Color
                             , b :: Color
                             , getTransform :: Matrix Double}
             | GradientPattern {  a:: Color
                                , b :: Color
                                , getTransform :: Matrix Double}
 deriving Show

patternAt :: Pattern -> Tuple Double -> Color
patternAt (StripePattern a b _) p = if mod (floor . getX $ p) 2 == 0
                                      then a
                                      else b
patternAt (GradientPattern a b _) p = a + fmap ((*) frac) dist
                                        where 
                                              dist :: Tuple Double
                                              dist = b - a
                                              frac :: Double
                                              frac = fromInteger (floor (getX p))
                                              
