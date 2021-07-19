module StateTest where
import           State
import qualified Test.HUnit      as TH
import qualified Test.QuickCheck as T

testCase51 = TH.assertEqual "ask" [(1::Double)] (eval (do
    State $ \s -> ([1 :: Double] <> s,[1 :: Double] <> s)
    ask) [])
