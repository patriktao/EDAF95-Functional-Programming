data CircList a :: CircList [a]

-- (Int, [Int])
-- [2,3,4,5,1]

perimeter :: CircList a -> In
cutrrentelem (CircList []) = 0
perimeter (CircList l) = length l 

currentelem :: CircList a -> Maybe a 
--currentelem (CircList []) = Nothing
--currentelem (CircList l) = Just $ head l
currentElemt (CircList l) = listToMaybe l

nextelem :: CircList a -> Maybe a
nextelem (CircList []) = Nothing
nextelem (CircList [a]) = Just a
nextelem (CircleList (x:xs)) = listToMaybe xs 
{-nextelem (CircList l) = case perimeter l of 
	1 -> Just $ head l 
	_ -> Just $ head $ tail l 
-}

previouselem :: CircList a -> Maybe a
previouselem (CircList []) = Nothing
previouselem (CircList [a]) = Just a
previouselem (CircList (x:xs)) = listToMaybe $ reverse xs 
-- vi vänder på hela och ta ut det första elementet
--previouselem (CircList a) = Just $ last a

insert :: a -> CircList a -> CircList a 
insert val (CircList l) = CircList $ l ++ [val]

delete :: Int -> CircList a -> CircList a
delete n (CircList l)
	| perimeter (CircList l) <= n = CircList [] --ifall det finns mindre än det man vill ta bort, returna en tom, alla elementen har tagits bort 
	| otherise = CircList $ drop n l

takefromCL :: Int -> CircList a -> [a]
takeFromCL n (CircList []) 	= [] 
takefromCL n (CircList l) 	= take n $ cycle l 
-- takefromCL n (CircList l) = take n l  
-- cycle ties a finite list into a circular one.

-- we generate all possible permutations of ys, where each permutation is rotated from the previous one. One of these rotated permutations ys then might matches xs. If then check (== xs) to each permutation of ys using map, we can see if they are equivalent. This returns a list of boolean values indicating whether each permutation is equal to xs or not. If any of the elements are True, then we return True, otherwise false.

equalCL :: CircList a -> CircList a -> Bool
equalCL (CircList xs) (CircList ys) 
	| length xs /= length ys 	= False 
	| null xs 					= False 
	| otherwise 				= or $ map (xs==) (permutations ys)

-- iterate takes in an initial element, and generates an infinite list
-- where each subsequent element is obtained by applying the function rotate to the
-- previous element.
permutations xs = take (length xs) (iterate rotate xs)

--moves the first value to last place, rotates to the left.
rotate [] = []
rotate (x:xs) = xs++[x]



{-
	Tre fall
	fall 1. de har inte samma längd returnera false
	fall 2. en av de är tomma
	fall 3. vi tar ut alla permutationer och checkar om en av dessa permutationer ys == xs genom map. Vi får ut en lista av boolean values, och om en av dessa är true returnerar vi true, annars false.
-}