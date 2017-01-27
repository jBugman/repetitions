{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Repetitions.Core

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "lastN 2 of 4" $ lastN 2 [1, 2, 3, 4] @?= ([3, 4] :: [Int])
  , testCase "lastN 1 of 0" $ lastN 1 [] @?= ([] :: [Int])
  ]

newtype IntList = IntList [Int] deriving (Eq, Show, Arbitrary)

properties :: TestTree
properties = testGroup "Properties"
  [
    testProperty "lastN X of Y" $ \(NonNegative x) (IntList xs) -> length (lastN x xs) <= x
  ]
