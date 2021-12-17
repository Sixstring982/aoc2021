module GridSpec (properties) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QuickCheck
import Test.Tasty.HUnit ((@?=), testCase)

import Grid (gridRow, gridCol)
import qualified Grid as Grid

properties :: TestTree
properties = testGroup "GridProperties" [
  getsGridRows,
  getsGridCols
  ]

getsGridRows = testCase "Can get a grid's rows" $
  [1, 2, 3] `compare` (gridRow 0 (Grid.fromLists [[1, 2, 3]])) @?= EQ

getsGridCols = testCase "Can get a grid's columns" $
  [2] `compare` (gridCol 1 (Grid.fromLists [[1, 2, 3]])) @?= EQ


