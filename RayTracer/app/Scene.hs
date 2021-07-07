module Scene where
{- 
This module is part of chapter 1 of The Ray Tracer Challenge

It's goal is too show how tuples can be used in order to calculate projectiles in an environment
-}    
import Tuples
import Canvas
import Colors
import Data.Array

data Projectile a = Projectile { position :: Tuple a, velocity :: Tuple a} 
 deriving Show

data Environment a = Environment { gravity :: Tuple a, wind :: Tuple a }
--- s = Projectile
--- a = Enviroment

p = Projectile (point 0.0 1.0 0.0) (normalize (vector 1.0 1.8 0.0))
e = Environment (vector 0.0 (-0.1) 0.0) (vector (-0.01) 0 0)

runScenario :: (Environment Double) -> Scenario (Projectile Double) (Projectile Double)
runScenario env = (Scenario $ \s0 -> let s1 = run env s0
                                     in (s0, s1)) 
                            where run env proj = let pos = position proj + velocity proj 
                                                     vel = velocity proj + gravity env + wind env
                                                 in Projectile pos vel 

scene :: (Environment Double) -> Int -> Scenario (Projectile Double) [Projectile Double]
scene env n = sequence (replicate n (runScenario env))

mapProjectile :: Canvas Int -> [Tuple Double] -> [(Int, Int)]
mapProjectile c tuples = map (\tup -> (round (getX tup), getHeight c - round (getY tup))) tuples


-- TODO: fix pixel array check imediately for out of bound!
mapProjectile' :: Canvas Int -> [Tuple Double] -> Array (Int, Int) Pixel
mapProjectile' c tuples = pixelArray ((0,0), (((getWidth c) - 1), ((getHeight c) - 1))) (map (\pos -> pixel (round (getX pos)) (round (getY pos)) (color 1.0 1.0 1.0 1.0)) tuples)


main :: IO ()
main = do
--   putStrLn "Enter the number of iterations: "
--   n <- getLine
--   putStrLn "Enter the number of scalar multiplier for velocity of projectile: "
--   v <- getLine
  let n = "1000"
      v = "11.25"
      result = map position (fst $ tick (scene e (read n)) (Projectile (position p) ((velocity p)* (pure (read v)))))
      hit = filter (\x -> getY x > 0) result
--   print (mapProjectile myCanvas hit)
  print (mapProjectile myCanvas hit)
  let newCanvas = (writePixels myCanvas (mapProjectile myCanvas hit) (color 1 1 1 1))
  let newCanvas' = (writePixels' myCanvas (mapProjectile' myCanvas hit))
  createPPM newCanvas "projectile.ppm"
--   createPPM newCanvas' "projectile2.ppm"
  if getY (last hit) > 0 && length hit == (read n)
      then putStrLn "Try more iterations or higher velocity"
      else do
          putStrLn "Took # iterations:"
          print (length hit)


newtype Scenario s a = Scenario { tick :: s -> (a, s) } 

instance Functor (Scenario s) where
 fmap f x = Scenario $ \s0 ->  let (a, s1) = (tick x s0)
                               in (f a, s1)

instance Applicative (Scenario s) where
 pure a = Scenario $ \s -> (a, s)
 sf <*> sa = Scenario $ \s0 -> let (fs, s1) = (tick sf s0)
                                   (a, s2) = (tick sa s1)
                               in (fs a, s2) 

instance Monad (Scenario s) where
 return a = pure a
 sa >>= asb = Scenario $ \s0 -> let (a, s1) = tick sa s0
                                in (tick (asb a) s1)
