module BitsPacketSpec (units) where

import BitsPacket (BitsPacket (..), evaluatePacket, parsePacket)
import Hex (HexString (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

units :: TestTree
units =
  testGroup
    "BitsPacket units"
    [ testParsesPackets,
      testEvaluatesPackets
    ]

testParsesPackets :: TestTree
testParsesPackets =
  testGroup "Parses packets" $
    [ testCase "D2FE28 parses as literal" $
        parsePacket (HexString "D2FE28") @?= (Literal 6 2021),
      testCase "38006F45291200 parses as operator" $
        parsePacket (HexString "38006F45291200")
          @?= (Operator 1 6 [Literal 6 10, Literal 2 20]),
      testCase "EE00D40C823060 parses as operator" $
        parsePacket (HexString "EE00D40C823060")
          @?= (Operator 7 3 [Literal 2 1, Literal 4 2, Literal 1 3]),
      testCase "8A004A801A8002F478 parses as operator" $
        parsePacket (HexString "8A004A801A8002F478")
          @?= (Operator 4 2 [Operator 1 2 [Operator 5 2 [Literal 6 15]]]),
      testCase "620080001611562C8802118E34 parses as operator" $
        parsePacket (HexString "620080001611562C8802118E34")
          @?= (Operator 3 0 [Operator 0 0 [Literal 0 10, Literal 5 11], Operator 1 0 [Literal 0 12, Literal 3 13]]),
      testCase "C0015000016115A2E0802F182340 parses as operator" $
        parsePacket (HexString "C0015000016115A2E0802F182340")
          @?= (Operator 6 0 [Operator 0 0 [Literal 0 10, Literal 6 11], Operator 4 0 [Literal 7 12, Literal 0 13]]),
      testCase "A0016C880162017C3686B18A3D4780 parses as operator" $
        parsePacket (HexString "A0016C880162017C3686B18A3D4780")
          @?= (Operator 5 0 [Operator 1 0 [Operator 3 0 [Literal 7 6, Literal 6 6, Literal 5 12, Literal 2 15, Literal 2 15]]])
    ]

testEvaluatesPackets :: TestTree
testEvaluatesPackets =
  let expectEquals hex v = evaluatePacket (parsePacket (HexString hex)) @?= v
   in testGroup "Evaluates packets" $
        [ testCase "C200B40A82 evaluation" $ expectEquals "C200B40A82" 3,
          testCase "04005AC33890 evaluation" $ expectEquals "04005AC33890" 54,
          testCase "880086C3E88112 evaluation" $ expectEquals "880086C3E88112" 7,
          testCase "CE00C43D881120 evaluation" $ expectEquals "CE00C43D881120" 9,
          testCase "D8005AC2A8F0 evaluation" $ expectEquals "D8005AC2A8F0" 1,
          testCase "F600BC2D8F evaluation" $ expectEquals "F600BC2D8F" 0,
          testCase "9C005AC2F8F0 evaluation" $ expectEquals "9C005AC2F8F0" 0,
          testCase "9C0141080250320F1802104A08 evaluation" $ expectEquals "9C0141080250320F1802104A08" 1
        ]
