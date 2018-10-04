module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified DeferredFolds.Unfoldr as Unfoldr


main =
  defaultMain $
  testGroup "All" $ [
    testProperty "List roundtrip" $ \ (list :: [Int]) ->
    list === toList (Unfoldr.foldable list)
    ,
    testProperty "take" $ \ (list :: [Int], amount) ->
    take amount list ===
    toList (Unfoldr.take amount (Unfoldr.foldable list))
    ,
    testProperty "takeWhile odd" $ \ (list :: [Int]) ->
    takeWhile odd list ===
    toList (Unfoldr.takeWhile odd (Unfoldr.foldable list))
  ]
