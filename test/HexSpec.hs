module HexSpec (units) where

import Hex (BinaryString (..), HexString (..), fromBinary, toBinary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

units :: TestTree
units =
  testGroup
    "Hex units"
    [ testConvertsToBinary,
      testConvertsFromBinary
    ]

testConvertsToBinary :: TestTree
testConvertsToBinary =
  testGroup "Converts to binary" $
    [ testCase "D2FE28 converts to binary" $
        toBinary (HexString "D2FE28")
          `compare` (BinaryString "110100101111111000101000") @?= EQ,
      testCase "38006F45291200 converts to binary" $
        toBinary (HexString "38006F45291200")
          `compare` (BinaryString "00111000000000000110111101000101001010010001001000000000") @?= EQ
    ]

testConvertsFromBinary :: TestTree
testConvertsFromBinary =
  testGroup "Converts from binary" $
    [ testCase "Converts zero from binary" $ fromBinary "0000" @?= 0,
      testCase "Converts 10 from binary" $ fromBinary "1010" @?= 10,
      testCase "Converts 8 from binary" $ fromBinary "1000" @?= 8
    ]
