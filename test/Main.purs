module Test.Main where

import Prelude

import ArraySplit (group, groups, permutations, splitAtElem, splitAtIndices, tails)
import Data.Array as A
import Data.Set (Set, fromFoldable, size)
import Effect (Effect)
import Test.QuickCheck ((===))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

orderings :: Int -> Int
orderings 0 = 0
orderings 1 = 1
orderings n = n * orderings (n - 1)

splitTwoIndices :: forall a. Int -> Int -> Array a -> Array (Array a)
splitTwoIndices i j xs =
  A.cons (A.take j xs # A.drop i) [ A.drop (max i j) xs ]
    # A.cons (A.take i xs)

small :: Int -> Int
small x | x > 0 = mod x 100
small x = mod (-x) 100

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
      test "quick permutations" do
        quickCheck \(xs :: Array Char) -> (size $ fromFoldable $ permutations xs) === (orderings $ A.length xs)

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
      test "quick group" do
        quickCheck \(xs :: Array Int) g -> (A.concat $ group g xs) === if 0 < g then xs else []

    suite "groups" do
      test "empty index, empty xs" do
        Assert.equal (groups [] ([] :: Array Int)) $ []
      test "empty index" do
        Assert.equal (groups [] [ 1, 2, 3 ]) $ []
      test "empty xs" do
        Assert.equal (groups [ 1, 2, 3 ] ([] :: Array Int)) $ []
      test "indicies 1,2,3, xs 1..9" do
        Assert.equal (groups [ 1, 2, 3 ] [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]) $ [ [ 1 ], [ 2, 3 ], [ 4, 5, 6 ] ]
      test "indicies 1 xs 1..9" do
        Assert.equal (groups [ 1 ] [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]) $ [ [ 1 ] ]
      test "indicies 1,5,12 xs 1..9" do
        Assert.equal (groups [ 1, 5, 12 ] [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]) $ [ [ 1 ], [ 2, 3, 4, 5, 6 ], [ 7, 8, 9 ] ]
      test "indicies 1,5,12,1 xs 1..9" do
        Assert.equal (groups [ 1, 5, 12, 1 ] [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]) $ [ [ 1 ], [ 2, 3, 4, 5, 6 ], [ 7, 8, 9 ] ]

    suite "splitAtElem" do
      test "quickcheck" do
        quickCheck \(a :: Int) b c -> splitAtElem a (b <> [ a ] <> c) === [ b, [ a ] <> c ]

    suite "tails" do
      test "empty" do
        Assert.equal (tails ([] :: Array Int)) []
      test "one" do
        Assert.equal (tails [ 1 ]) []
      test "two" do
        Assert.equal (tails [ "a", "b" ]) [ [ "b" ] ]
      test "quickcheck tails" do
        quickCheck
          \(xs :: Array Int) ->
            let
              xs2 = xs <> [ 1, 2 ]
            in
              (tails xs2 # fromFoldable) ===
                (fromFoldable $ map (\x -> A.reverse xs2 # A.take x # A.reverse) (A.range 1 (A.length xs + 1)))

    suite "splitAtIndices" do
      test "quick multi concat" do
        quickCheck \index (xs :: Array Char) -> (splitAtIndices index xs # A.concat) === xs
      test "quick multi two indexes" do
        quickCheck \i j (xs :: Array Char) -> (splitAtIndices [ small i, small j ] xs) === splitTwoIndices (small i) (small j) xs

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