module ArraySplit where

import Prelude

import Data.Array (length)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)

permutations :: forall a. Array a -> Array (Array a)
permutations [] = []
permutations [ x ] = [ [ x ] ]
permutations xs = do
  p <- permutations (Array.drop 1 xs)
  i <- Array.range 0 ((Array.length xs) - 1)
  case Array.head xs of
    Just x -> maybe [] pure (Array.insertAt i x p)
    _ -> []

group :: forall a. Int -> Array a -> Array (Array a)
group _ [] = []
group n l
  | n > 0 = [ (Array.take n l) ] <> ((group n (Array.drop n l)))
  | otherwise = []


--divvy 5 5 [1..20] == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]     
--
splitAtElems :: forall a. Ord a => Array a -> Array a -> Array (Array a)
splitAtElems splitters xs =
  let
    subs = Array.foldl
      (\acc x -> if Array.elem x splitters then { head: [ x ], tail: acc.tail <> [ acc.head ] } else acc { head = Array.snoc acc.head x })
      { head: [], tail: [] }
      xs
  in
    subs.tail <> [ subs.head ]
 
splitAtElem :: forall a. Ord a => a -> Array a -> Array (Array a)
splitAtElem splitter xs = splitAtElems [splitter] xs

--splitAtSubList do notation case [x c v]

--split (onSublist "xyz") "aazbxyzcxd" == ["aazb","xyz","cxd"]

tails :: forall a. Array a -> Array (Array a)
tails x | length x < 2 = []
tails xs = go (pure (Array.drop 1 xs)) (Array.drop 1 xs)
  where
  go :: Array (Array a) -> Array a -> Array (Array a)
  go acc ys =
    case Array.drop 1 ys of
      [] -> acc
      rest -> go (Array.cons rest acc) rest
