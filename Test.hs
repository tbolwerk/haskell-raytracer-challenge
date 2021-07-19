import           Control.Monad
import           Control.Monad.ST
import           Data.Array         as A
import           Data.Array.ST
import           Data.Array.Unboxed


data Color = Color { red :: Double, green :: Double, blue ::Double }
 deriving Show


bubbleSort :: Array Int Double -> UArray Int Double
bubbleSort myArray = runSTUArray $ do
   stArray <- thaw myArray
   let end = (snd . A.bounds) myArray
   forM_ [0 .. end] $ \i -> do
     forM_ [0 .. end] $ \j -> do
       val <- readArray stArray j
       writeArray stArray i ( val)
   return stArray



