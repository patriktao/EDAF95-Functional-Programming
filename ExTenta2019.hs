{- 1 -}
-- zipWith applies a binary function to two list and produces a new list as a result
-- map applies a function to each element of a list, returning a new list
-- zipWith map applies a binary function to elements from two lists (from map function), and then
-- maps another function to the result of each pair-wise operation.

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- map :: (a -> b) -> [a] -> [b]
-- zipWith map :: [(a -> b -> c)] -> [[a]] -> [[b]] -> [[c]]

-- map takes in one function and one list, and produces a list with those function applied to
-- each element in the list
-- map zipWith :: [(a -> b -> c)] -> [[[a]] -> [[b]] -> [[c]]]

-- Therefore, the type signature of map zipWith can be summarized as a function that takes a list of binary functions and multiple pairs of lists, and returns a list of lists.

-- map . zipWith :: (a -> b -> c) -> [[a]] -> [[b] -> [c]]
-- takes a (zipping) function and a list of second arguments for map (i.e. lists), yielding a list of partially applied zipWiths waiting for the second argument (i.e. a list)

{- 2 -}
permutations :: [a] -> [[a]]
permutations [] = []
permutations list = [x : xs | x <- list, xs <- permutate $ delete x list]

{- 3 -}
g :: [[Int]] -> [[Int]]
g [] = []
-- g list = [tail x | x <- list, not $ null x, odd $ head x]
g list = map tail (filter (odd . head) $ filter (not . null) list)

{- 4 -}
-- foldr is used to reduce a list to a single value by repeateadly applying a binary function to ro the elements of the list, starting from the last element and working towards left.
-- It takes a function, an initital value, and the list to be folded.
okänd :: [a] -> [a]
okänd xs = foldr (++) [] (map (\y -> [y]) xs)

-- the following function reduces a list to a single vlaue by repeatedly applying a binary function to the elemnents of the list, starting from the last element, and working towards left. Each element of the list xs are transformed by the map function to a list containing that particular element. This list containing one element are then concatenated to the initial value and this is continiously done until the last element, which results to a list of those values concatenated together.
-- If xs = [1,2,3] the map turns it into [[1], [2], [3]], (++) is list concatenation which then then go through the elements from left to right and turns it back to [1,2,3]

{- 5: Pattern Matching -}
oneOf :: Bool -> Bool -> Bool -> Bool
oneOf False False True = True
oneOf True False False = True
oneOf False True False = True
oneOf _ _ _ = False

{- 6: Bind -}
eliminatem n [] g = Just g
eliminatem n (x : xs) g = eliminate n x g >>= eliminatem n xs

--a) 
-- g:: Grid
-- eliminate a 
eliminatem -> a -> [b] -> Grid -> Maybe Grid

--b)
-- eliminate a
elimatem -> a -> [b] -> Grid -> [Grid]
eliminatem n [] g = [g]
eliminatem n (x : xs) g = eliminate n x g >>= eliminatem n xs

--c)
--Yes