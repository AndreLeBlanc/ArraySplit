module ArraySplit
  ( permutations
  , group
  , groups
  , chop
  , divvy
  , tails
  , splitAtElems
  , splitAtElem
  , splitAtIndices
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..), maybe, fromMaybe)

-- | Creates an `Array` containing all the permutations of an `Array`.
-- |
-- | ```purescript
-- | permutations [1] = [1]
-- | permutations [1, 2, 3] = [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-- | ```
-- |
permutations :: forall a. Array a -> Array (Array a)
permutations [] = []
permutations [ x ] = [ [ x ] ]
permutations xs = do
  p <- permutations (A.drop 1 xs)
  i <- A.range 0 ((A.length xs) - 1)
  case A.head xs of
    Just x -> maybe [] pure (A.insertAt i x p)
    _ -> []

-- | Splits an `Array` into subarrays of size n.
-- |
-- | ```purescript
-- | group 1 [1,2,3] = [[1], [2], [3]]
-- | group 2 [1, 2, 3, 4, 5] = [[1, 2],[3, 4],[5]]
-- | ```
-- |
group :: forall a. Int -> Array a -> Array (Array a)
group _ [] = []
group n l
  | n > 0 = [ (A.take n l) ] <> ((group n (A.drop n l)))
  | otherwise = []

-- | Splits an `Array` into subarrays of the sizes given by indices `Array`. 
-- | Elements that don't fit in the subarrays are excluded.
-- |
-- | ```purescript
-- | groups [1, 2] [1,2,3] = [[1], [2, 3]]
-- | groups [2, 3] [1, 2, 3, 4, 5, 6] = [[1, 2],[3, 4, 5]]
-- | ```
-- |
groups :: forall a. Array Int -> Array a -> Array (Array a)
groups _ [] = []
groups indices xs =
  do
    { head, tail } <- A.uncons indices
    [ A.take head xs ] <> groups tail (A.drop head xs) # pure
    # fromMaybe []

-- | 
-- | A useful recursion pattern for processing an `Array` to produce a new `Array`, often used for "chopping" up the input `Array`.
-- | chop is called with some function that will consume an initial prefix of the `Array` and produce a record containing the 
-- | result of applying the function on the prefix, as well as the tail of the `Array`.
-- |
chop :: forall a b. (Array a -> { el :: b, as :: Array a }) -> Array a -> Array b
chop _ [] = []
chop f as =
  let
    { el, as } = f as
  in
    chop f as
      # A.cons el

-- | Divides up an input `Array` into a set of subarrays, according to 'n' and 'm'
-- |  input specifications you provide. Each subarray will have 'n' items, and the
-- |  start of each subarray will be offset by 'm' items from the previous one.
-- | ```purescript
-- | divvy 5 5 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20] = [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
-- | tails [1] = []
-- | ```
-- |  In the case where a source array's trailing elements do no fill an entire
-- |  subarray, those trailing elements will be dropped.
-- |
-- | ```purescript
-- | divvy 5 2 [1..10] == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
-- | ```
divvy :: forall a. Int -> Int -> Array a -> Array (Array a)
divvy _ _ [] = []
divvy n m xs = A.filter (\ws -> (n == A.length ws)) choppedl
  where
  choppedl = chop (\x -> { el: A.take n x, as: A.drop m x }) xs

-- | Creates an Array containing all the tails of a given `Array`.
-- |
-- | ```purescript
-- | tails [1,2,3,4,5,6] = [[6],[5,6],[4,5,6],[3,4,5,6],[2,3,4,5,6]]
-- | tails [1] = []
-- | ```
-- |
tails :: forall a. Array a -> Array (Array a)
tails x | A.length x < 2 = []
tails xs = go (pure (A.drop 1 xs)) (A.drop 1 xs)
  where
  go :: Array (Array a) -> Array a -> Array (Array a)
  go acc ys =
    case A.drop 1 ys of
      [] -> acc
      rest -> go (A.cons rest acc) rest

-- | Splits an `Array` into subarrays, split at instances of elements in the splitters `Array`.
-- |
-- | ```purescript
-- | splitAtElems [1,2] [4,3,1,3,2,1,4,5,5] = [[4,3,1],[3,2],[1],[4,5,5]]
-- | splitAtElems [1,2] [1,4,3,1,3,2,1,4,5,5] =[[1],[4,3,1],[3,2],[1],[4,5,5]]
-- | splitAtElems [7] [1,4,3,1,3,2,1,4,5,5] = [[1,4,3,1,3,2,1,4,5,5]]
-- | ```
-- |
splitAtElems :: forall a. Eq a => Array a -> Array a -> Array (Array a)
splitAtElems splitters xs =
  let
    { h, t } = A.foldl
      (\acc x -> if A.elem x splitters then { h: [], t: acc.t <> [ A.snoc acc.h x ] } else acc { h = A.snoc acc.h x })
      { h: [], t: [] }
      xs
  in
    A.snoc t h

-- | Splits an `Array` into subarrays, split at instances of splitter. The same as splitAtElems [splitter] xs.
-- |
-- | ```purescript
-- | splitAtElem  [4,3,1,3,2,1,4,5,5] = [[4,3,1],[3,2],[1],[4,5,5]]
-- | splitAtElem  [1,4,3,1,3,2,1,4,5,5] =[[1],[4,3,1],[3,2],[1],[4,5,5]]
-- | splitAtElem [1,4,3,1,3,2,1,4,5,5] = [[1,4,3,1,3,2,1,4,5,5]]
-- | ```
-- |
splitAtElem :: forall a. Eq a => a -> Array a -> Array (Array a)
splitAtElem splitter xs = splitAtElems [ splitter ] xs

-- | Splits an `Array` into subarrays. Splits at indices given in the argument splitter. 
-- |
-- | ```purescript
-- | splitAtIndices  [3,5] [1,2,3,4,5,6,7,8] = [[1,2,3],[4,5],[6,7,8]]
-- | splitAtIndices  [] [1,2,3,4,5,6,7,8]  = [[1,2,3,4,5,6,7,8]]
-- | splitAtIndices  [4,7,10] [1,2,3,4,5,6,7,8] = [[1,2,3,4],[5,6,7],[8]]
-- | ```
-- |
splitAtIndices :: forall a. Array Int -> Array a -> Array (Array a)
splitAtIndices _ [] = []
splitAtIndices [] xs = [ xs ]
splitAtIndices indices xs =
  do
    { head, tail } <- A.uncons indices
    let newTail = A.foldl (\acc t -> A.snoc acc (t - head)) [] tail
    let { before, after } = A.splitAt head xs
    A.cons before (splitAtIndices newTail after) # pure
    # fromMaybe []
