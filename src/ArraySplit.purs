module ArraySplit where

import Prelude

import Data.Array (length)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe, fromMaybe)

permutations :: forall a. Array a -> Array (Array a)
permutations [] = []
permutations [ x ] = [ [ x ] ]
permutations xs = do
  p <- permutations (A.drop 1 xs)
  i <- A.range 0 ((A.length xs) - 1)
  case A.head xs of
    Just x -> maybe [] pure (A.insertAt i x p)
    _ -> []

group :: forall a. Int -> Array a -> Array (Array a)
group _ [] = []
group n l
  | n > 0 = [ (A.take n l) ] <> ((group n (A.drop n l)))
  | otherwise = []

groups :: forall a. Array Int -> Array a -> Array (Array a)
groups [] _ = []
groups _ [] = []
groups indicies xs =
  do
    { head, tail } <- A.uncons indicies
    [ A.take head xs ] <> groups tail (A.drop head xs) # pure
    # fromMaybe []

chop :: forall a b. (Array a -> { el :: b, as :: Array a }) -> Array a -> Array b
chop _ [] = []
chop f as =
  let
    { as, el } = f as
  in
    chop f as
      # A.cons el

divvy :: forall a. Int -> Int -> Array a -> Array (Array a)
divvy _ _ [] = []
divvy n m xs = A.filter (\ws -> (n == A.length ws)) choppedl
  where
  choppedl = chop (\x -> { el: A.take n x, as: A.drop m x }) xs

tails :: forall a. Array a -> Array (Array a)
tails x | length x < 2 = []
tails xs = go (pure (A.drop 1 xs)) (A.drop 1 xs)
  where
  go :: Array (Array a) -> Array a -> Array (Array a)
  go acc ys =
    case A.drop 1 ys of
      [] -> acc
      rest -> go (A.cons rest acc) rest

splitAtElems :: forall a. Ord a => Array a -> Array a -> Array (Array a)
splitAtElems splitters xs =
  let
    subs = A.foldl
      (\acc x -> if A.elem x splitters then { head: [ x ], tail: acc.tail <> [ acc.head ] } else acc { head = A.snoc acc.head x })
      { head: [], tail: [] }
      xs
  in
    subs.tail <> [ subs.head ]

splitAtElem :: forall a. Ord a => a -> Array a -> Array (Array a)
splitAtElem splitter xs = splitAtElems [ splitter ] xs

splitAtSubList :: forall a. Eq a => Array a -> Array a -> Array (Array a)
splitAtSubList sub xs =
  let
    { h, t } = A.foldl (\acc x -> if A.elem x sub then { h: [], t: A.snoc acc.t acc.h } else acc { h = A.snoc acc.h x }) { h: [], t: [] } xs
  in
    A.cons h t

splitAtIndices :: forall a. Array Int -> Array a -> Array (Array a)
splitAtIndices [] xs = [ xs ]
splitAtIndices indicies xs =
  do
    { head, tail } <- A.uncons indicies
    let newTail = A.foldl (\acc t -> A.cons (t - head) acc) [] tail
    let { before, after } = A.splitAt head xs
    A.cons before (splitAtIndices newTail after) # pure
    # fromMaybe []
