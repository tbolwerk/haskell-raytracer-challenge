import TuplesTest
import CanvasTest
import ColorsTest
import MatricesTest
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

main = defaultMain tests

tests = [
        testGroup "Tuples" [
                  testCase "testCase1: A tuple with w=1.0 is a point" testCase1
                , testCase "testCase2: A tuple with w=0.0 is a vector" testCase2
                , testCase "testCase3: Creates a tuple with w=0.0" testCase3
                , testCase "testCase4: Creates a tuple with w=1.0" testCase4
                , testCase "testCase5: Adding two tuples" testCase5
                , testCase "testCase6: Subtracting two points" testCase6
                , testCase "testCase7: Subtracting a vector from a point" testCase7
                , testCase "testCase8: Subtracting two vectors" testCase8
                , testCase "testCase9: Subtracting a vector from the zero vector" testCase9
                , testCase "testCase10: Negating a tuple" testCase10
                , testCase "testCase11: Multiplying a tuple by a scalar" testCase11
                , testCase "testCase12: Multiplying a tuple by a fraction" testCase12
                , testCase "testCase13: Dividing a tuple by a scalar" testCase13
                , testCase "testCase14: Computing the magnitude of vector (1,0,0)" testCase14
                , testCase "testCase15: Computing the magnitude of vector (0,1,0)" testCase15
                , testCase "testCase16: Computing the magnitude of vector (0,0,1)" testCase16
                , testCase "testCase17: Computing the magnitude of vector (1,2,3)" testCase17
                , testCase "testCase18: Computing the magnitude of vector (-1,-2,-3)" testCase18
                , testCase "testCase19: Normalizing vector (4,0,0) gives (1,0,0)" testCase19
                , testCase "testCase20: Normalizing vector (1,2,3) gives (1/sqrt 14,2/sqrt 14,3/sqrt 14)" testCase20
                , testCase "testCase21: The magnitude of a normalized vector (1,2,3) gives 1" testCase21
                , testCase "testCase22: The dot product of two tuples" testCase22
                , testCase "testCase23: The dot product of two vectors (a,b)" testCase23
                , testCase "testCase24: The dot product of two vectors (b,a)" testCase24
                , testCase "testCase25: Colors are (RED, green, blue) tuples" testCase25
                , testCase "testCase26: Colors are (red, GREEN, blue) tuples" testCase26
                , testCase "testCase27: Colors are (red, green, BLUE) tuples" testCase27
                , testCase "testCase28: Adding colors" testCase28
                , testCase "testCase29: Subtracting colors" testCase29
                , testCase "testCase30: Multiplying a color by a scalar" testCase30
                , testCase "testCase31: Multiplying colors" testCase31
                , testProperty "prop_Point" prop_Point
                , testProperty "prop_Vector" prop_Vector
                , testProperty "prop_Tuple_Equal" prop_Tuple_Equal
                , testProperty "prop_Tuple_Addition" prop_Tuple_Addition
                , testProperty "prop_Tuple_Subtraction" prop_Tuple_Subtraction
                , testProperty "prop_Tuple_Negate" prop_Tuple_Negate
                , testProperty "prop_Tuple_Scalar_Multiplication" prop_Tuple_Scalar_Multiplication
                , testProperty "prop_Tuple_Scalar_Division" prop_Tuple_Scalar_Division
                , testProperty "prop_Tuple_Magnitude" prop_Tuple_Magnitude
                , testProperty "prop_Tuple_Normalize" prop_Tuple_Normalize
                , testProperty "prop_Tuple_Dot_Product" prop_Tuple_Dot_Product
                , testProperty "prop_Vector_Cross_Product" prop_Vector_Cross_Product
            ],
        testGroup "Canvas" [
                  testProperty "prop_Canvas_Create" prop_Canvas_Create
                , testProperty "prop_Canvas_PixelAt" prop_Canvas_PixelAt
                , testProperty "prop_Canvas_WritePixel" prop_Canvas_WritePixel
                , testProperty "prop_Canvas_PPM_EndsWithNewLine" prop_Canvas_PPM_EndsWithNewLine
                , testCase "testCase32: Every pixel of initial canvas is black" testCase32
                , testCase "testCase33: Width of canvas is correctly set" testCase33
                , testCase "testCase34: Height of canvas is correctly set" testCase34
                , testCase "testCase35: Pixel_at(c,2,3) = red" testCase35
            ],
        testGroup "Matrices" [
              testCase "Construction and inspecting a 4x4 matrix M[0,0] = 1" testCase36
            , testCase "Construction and inspecting a 4x4 matrix M[0,3] = 4" testCase37
            , testCase "Construction and inspecting a 4x4 matrix M[1,0] = 5.5" testCase38
            , testCase "Construction and inspecting a 4x4 matrix M[1,2] = 7.5" testCase39
            , testCase "Construction and inspecting a 4x4 matrix M[2,2] = 11" testCase40
            , testCase "Construction and inspecting a 4x4 matrix M[3,0] = 13.5" testCase41
            , testCase "Construction and inspecting a 4x4 matrix M[3,2] = 15.5" testCase42
        ]
    ]





