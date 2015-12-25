module Test where

import Test.Tasty
import Test.Tasty.HUnit

import Repetitions.Core

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "lastN 2 of 4" $ lastN 2 [1, 2, 3, 4] @?= ([3, 4] :: [Int])
  , testCase "lastN 1 of 0" $ lastN 1 [] @?= ([] :: [Int])
  ]
