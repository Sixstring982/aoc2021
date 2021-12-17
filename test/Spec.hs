import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified GridSpec as GridSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [GridSpec.properties]
