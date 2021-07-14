module StateTest where
import qualified Test.QuickCheck as T
import qualified Test.HUnit as TH
import State

testCase51 = TH.assertEqual "ask" [(1::Double)] (eval (do 
    State $ \s -> ([1 :: Double] <> s,[1 :: Double] <> s)
    ask) [])