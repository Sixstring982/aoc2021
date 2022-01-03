import qualified BitsPacketSpec as BitsPacketSpec
import qualified HexSpec as HexSpec
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ HexSpec.units,
      BitsPacketSpec.units
    ]
