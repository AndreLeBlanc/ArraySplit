module Test.Main where

import Prelude

import ArraySplit (group, splitAtElem, permutations, tails)
import Control.Monad.List.Trans (uncons)
import Data.Array as A
import Data.Maybe (fromMaybe)
import Data.Set (fromFoldable, Set)
import Effect (Effect)
import Test.QuickCheck (Result, (===))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

main ∷ Effect Unit
main =
  runTest do
    suite "permutations" do
      test "empty" do
        Assert.equal (permutations [] :: Array (Array String)) $ []
      test "singelton" do
        Assert.equal (permutations [ 1 ]) $ [ [ 1 ] ]
      test "duo" do
        Assert.equal (permutations [ 1, 2 ]) $ [ [ 1, 2 ], [ 2, 1 ] ]
      test "triplets" do
        Assert.equal (permutations [ 1, 2, 3 ] # fromFoldable) $ fromFoldable [ [ 1, 2, 3 ], [ 1, 3, 2 ], [ 2, 1, 3 ], [ 2, 3, 1 ], [ 3, 1, 2 ], [ 3, 2, 1 ] ]
      test "DCODE" do
        Assert.equal (permutations [ "D", "C", "O", "D", "E" ] # fromFoldable) $ permsDCODE

    suite "group" do
      test "empty" do
        Assert.equal (group 0 ([] :: Array Int)) []
      test "empty large group" do
        Assert.equal (group 199 ([] :: Array Int)) []
      test "123 large group" do
        Assert.equal (group 199 [ 1, 2, 3 ]) [ [ 1, 2, 3 ] ]
      test "group single" do
        Assert.equal (group 1 [ 1, 2, 3 ]) [ [ 1 ], [ 2 ], [ 3 ] ]
      test "group three" do
        Assert.equal (group 3 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]) [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ], [ 10, 11, 12 ] ]
      test "group three uneven" do
        Assert.equal (group 3 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ]) [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ], [ 10, 11, 12 ], [ 13 ] ]
    
    suite "splitAtElem" do
      test "quickcheck" do
        quickCheck (\(a :: Int)  b c->  splitAtElem a  (b <> [a] <> c) === [ b, [a] <> c])

    suite "tails" do
      test "empty" do
        Assert.equal (tails ([] :: Array Int)) []
      test "one" do
        Assert.equal (tails [ 1 ]) []
      test "two" do
        Assert.equal (tails [ "a", "b" ]) [ [ "b" ] ]
      test "three" do
        Assert.equal (tails [ "a", "b", "c" ]) [ [ "c" ], [ "b", "c" ] ]
      test "ten" do
        Assert.equal (tails [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 ]) [ [ 0 ], [ 9, 0 ], [ 8, 9, 0 ], [ 7, 8, 9, 0 ], [ 6, 7, 8, 9, 0 ], [ 5, 6, 7, 8, 9, 0 ], [ 4, 5, 6, 7, 8, 9, 0 ], [ 3, 4, 5, 6, 7, 8, 9, 0 ], [ 2, 3, 4, 5, 6, 7, 8, 9, 0 ] ]

permsDCODE :: Set (Array String)
permsDCODE = fromFoldable
  [ [ "D", "C", "O", "D", "E" ]
  , [ "C", "D", "O", "D", "E" ]
  , [ "O", "D", "C", "D", "E" ]
  , [ "D", "O", "C", "D", "E" ]
  , [ "C", "O", "D", "D", "E" ]
  , [ "O", "C", "D", "D", "E" ]
  , [ "D", "D", "O", "C", "E" ]
  , [ "O", "D", "D", "C", "E" ]
  , [ "D", "O", "D", "C", "E" ]
  , [ "C", "D", "D", "O", "E" ]
  , [ "D", "C", "D", "O", "E" ]
  , [ "D", "D", "C", "O", "E" ]
  , [ "E", "D", "C", "O", "D" ]
  , [ "D", "E", "C", "O", "D" ]
  , [ "C", "E", "D", "O", "D" ]
  , [ "E", "C", "D", "O", "D" ]
  , [ "D", "C", "E", "O", "D" ]
  , [ "C", "D", "E", "O", "D" ]
  , [ "C", "D", "O", "E", "D" ]
  , [ "D", "C", "O", "E", "D" ]
  , [ "O", "C", "D", "E", "D" ]
  , [ "C", "O", "D", "E", "D" ]
  , [ "D", "O", "C", "E", "D" ]
  , [ "O", "D", "C", "E", "D" ]
  , [ "O", "E", "C", "D", "D" ]
  , [ "E", "O", "C", "D", "D" ]
  , [ "C", "O", "E", "D", "D" ]
  , [ "O", "C", "E", "D", "D" ]
  , [ "E", "C", "O", "D", "D" ]
  , [ "C", "E", "O", "D", "D" ]
  , [ "D", "E", "O", "C", "D" ]
  , [ "E", "D", "O", "C", "D" ]
  , [ "O", "D", "E", "C", "D" ]
  , [ "D", "O", "E", "C", "D" ]
  , [ "E", "O", "D", "C", "D" ]
  , [ "O", "E", "D", "C", "D" ]
  , [ "D", "E", "D", "C", "O" ]
  , [ "E", "D", "D", "C", "O" ]
  , [ "D", "D", "E", "C", "O" ]
  , [ "D", "E", "C", "D", "O" ]
  , [ "E", "D", "C", "D", "O" ]
  , [ "C", "D", "E", "D", "O" ]
  , [ "D", "C", "E", "D", "O" ]
  , [ "E", "C", "D", "D", "O" ]
  , [ "C", "E", "D", "D", "O" ]
  , [ "C", "D", "D", "E", "O" ]
  , [ "D", "C", "D", "E", "O" ]
  , [ "D", "D", "C", "E", "O" ]
  , [ "O", "D", "E", "D", "C" ]
  , [ "D", "O", "E", "D", "C" ]
  , [ "E", "O", "D", "D", "C" ]
  , [ "O", "E", "D", "D", "C" ]
  , [ "D", "E", "O", "D", "C" ]
  , [ "E", "D", "O", "D", "C" ]
  , [ "E", "D", "D", "O", "C" ]
  , [ "D", "E", "D", "O", "C" ]
  , [ "D", "D", "E", "O", "C" ]
  , [ "D", "O", "D", "E", "C" ]
  , [ "O", "D", "D", "E", "C" ]
  , [ "D", "D", "O", "E", "C" ]
  ]