# ArraySplit

Additional utility functions for purescript arrays.

# Documentation

\### \[#\](#v:permutations)permutations

permutations :: forall a. Array a -> Array (Array a) Creates an \`Array\` containing all the permutations of an \`Array\`. permutations \[1\] = \[1\] permutations \[1, 2, 3\] = \[\[1,2,3\],\[2,1,3\],\[2,3,1\],\[1,3,2\],\[3,1,2\],\[3,2,1\]\]

\### \[#\](#v:group)group

group :: forall a. Int -> Array a -> Array (Array a) Splits an \`Array\` into subarrays of size n. group 1 \[1,2,3\] = \[\[1\], \[2\], \[3\]\] group 2 \[1, 2, 3, 4, 5\] = \[\[1, 2\],\[3, 4\],\[5\]\]

\### \[#\](#v:groups)groups

groups :: forall a. Array Int -> Array a -> Array (Array a) Splits an \`Array\` into subarrays of the sizes given by indices \`Array\`. Elements that don't fit in the subarrays are excluded. groups \[1, 2\] \[1,2,3\] = \[\[1\], \[2, 3\]\] groups \[2, 3\] \[1, 2, 3, 4, 5, 6\] = \[\[1, 2\],\[3, 4, 5\]\]

\### \[#\](#v:chop)chop

chop :: forall a b. (Array a -> { as :: Array a, el :: b }) -> Array a -> Array b A useful recursion pattern for processing an \`Array\` to produce a new \`Array\`, often used for "chopping" up the input \`Array\`. chop is called with some function that will consume an initial prefix of the \`Array\` and produce a record containing the result of applying the function on the prefix, as well as the tail of the \`Array\`.

\### \[#\](#v:divvy)divvy

divvy :: forall a. Int -> Int -> Array a -> Array (Array a) Divides up an input \`Array\` into a set of subarrays, according to 'n' and 'm' input specifications you provide. Each subarray will have 'n' items, and the start of each subarray will be offset by 'm' items from the previous one. divvy 5 5 \[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20\] = \[\[1,2,3,4,5\],\[6,7,8,9,10\],\[11,12,13,14,15\],\[16,17,18,19,20\]\] tails \[1\] = \[\] In the case where a source array's trailing elements do no fill an entire subarray, those trailing elements will be dropped. divvy 5 2 \[1..10\] == \[\[1,2,3,4,5\],\[3,4,5,6,7\],\[5,6,7,8,9\]\]

\### \[#\](#v:tails)tails

tails :: forall a. Array a -> Array (Array a) Creates an Array containing all the tails of a given \`Array\`. tails \[1,2,3,4,5,6\] = \[\[6\],\[5,6\],\[4,5,6\],\[3,4,5,6\],\[2,3,4,5,6\]\] tails \[1\] = \[\]

\### \[#\](#v:splitAtElems)splitAtElems

splitAtElems :: forall a. Eq a => Array a -> Array a -> Array (Array a) Splits an \`Array\` into subarrays, split at instances of elements in the splitters \`Array\`. splitAtElems \[1,2\] \[4,3,1,3,2,1,4,5,5\] = \[\[4,3,1\],\[3,2\],\[1\],\[4,5,5\]\] splitAtElems \[1,2\] \[1,4,3,1,3,2,1,4,5,5\] =\[\[1\],\[4,3,1\],\[3,2\],\[1\],\[4,5,5\]\] splitAtElems \[7\] \[1,4,3,1,3,2,1,4,5,5\] = \[\[1,4,3,1,3,2,1,4,5,5\]\]

\### \[#\](#v:splitAtElem)splitAtElem

splitAtElem :: forall a. Eq a => a -> Array a -> Array (Array a) Splits an \`Array\` into subarrays, split at instances of splitter. The same as splitAtElems \[splitter\] xs. splitAtElem \[4,3,1,3,2,1,4,5,5\] = \[\[4,3,1\],\[3,2\],\[1\],\[4,5,5\]\] splitAtElem \[1,4,3,1,3,2,1,4,5,5\] =\[\[1\],\[4,3,1\],\[3,2\],\[1\],\[4,5,5\]\] splitAtElem \[1,4,3,1,3,2,1,4,5,5\] = \[\[1,4,3,1,3,2,1,4,5,5\]\]

\### \[#\](#v:splitAtIndices)splitAtIndices

splitAtIndices :: forall a. Array Int -> Array a -> Array (Array a) Splits an \`Array\` into subarrays. Splits at indices given in the argument splitter. splitAtIndices \[3,5\] \[1,2,3,4,5,6,7,8\] = \[\[1,2,3\],\[4,5\],\[6,7,8\]\] splitAtIndices \[\] \[1,2,3,4,5,6,7,8\] = \[\[1,2,3,4,5,6,7,8\]\] splitAtIndices \[4,7,10\] \[1,2,3,4,5,6,7,8\] = \[\[1,2,3,4\],\[5,6,7\],\[8\]\]

