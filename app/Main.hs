module Main where


import qualified Chapter1
import qualified Chapter4
import qualified Chapter5
import qualified Chapter6
import qualified Chapter7
import qualified Chapter10

main = do
    -- putStrLn "Start rendering Chapter 1..."
    -- chapter1
    -- putStrLn "Finished render Chapter 1!"
    -- putStrLn "Start rendering Chapter 4..."
    -- chapter4
    -- putStrLn "Finished render Chapter 4!"
    -- putStrLn "Start rendering Chapter 5..."
    -- chapter5
    -- putStrLn "Finished render Chapter 5!"
    -- putStrLn "Start rendering Chapter 6..."
    -- chapter6
    -- putStrLn "Finished render Chapter 6!"
    -- putStrLn "Start rendering Chapter 7..."
    -- chapter7
    -- putStrLn "Finished render Chapter 7!"
    putStrLn "Start rendering Chapter 10..."
    chapter10
    putStrLn "Finished rendering all chapters!"

chapter1 = Chapter1.main
chapter4 = Chapter4.main
chapter5 = Chapter5.main
chapter6 = Chapter6.main
chapter7 = Chapter7.main
chapter10 = Chapter10.main
