

-- 1. Point-free

f x y = x / (8 - y)
f = flip $ (flip (/)) . (8 -)   


g x y = [x z | z <- [5,8..y]]
g x y =  map x (take y (iterate (+3) 5))
g =  flip $ (flip map) . (flip take $ iterate (+3) 5) -- y x
g = flip $ (flip map) . (flip takeWhile $ iterate (+3) 5) . (>=)) 

-- (flip takeWhile $ iterate (+3) 5) . (>=), takes an argument y and compares that
-- to each element in the iterate list, until it returns false.

-- vi vill enumerera från 5, 8, 11, +3
-- och för varje tal applicera x

-- 2. Explain the meaning and differences between the three type declarations: type, newtype, and data. 

{-
Type - type synonym or aliasis aimed to make code more readable, maintainable, modular, and expressive. Alternative names for type

Data - creates a new algebraic data type that allow multiple constructors and field types, can be used to create complex data structures, can combine existing types,.

Newtype - creates a data type with one single constructor wrapped around an existing type.

-}

--3 
{-
3. Consider the following function:

eliminatem n [] g = Just g
eliminatem n (s:ss) g = eliminate n s g >>= eliminatem n ss
-}

--3.1. (0.4p)
--Given that the type of g is Grid, g :: Grid
--write the signature for eliminatem. Assume the most generic type for eliminate.
eliminatem :: a -> [b] -> Grid -> Maybe Grid

-- 3.2.
--How would your answer look like if the first line were changed to eliminatem n [] g = [g]
elimatnem :: a -> [b] -> Grid = [Grid]


-- 3.3	
--Would the second line of the definition be correct after this change? Answer YES or NO, and motivate.
-- Yes it would work, since it will be a another monad, which will return [Grid].


-- 4: Redefine map f and filter f using foldr.
map f = foldr (\x xs -> f x : xs) []
filter f  = foldr (\x xs -> if f x then x : xs else xs) []

--5: What is the effect and type of?

-- uncurry ($)
uncurry :: (a -> b -> c) -> (a, b) -> c
($) :: (a->b) -> a -> b
-- Eftersom ($) har in två argumenter (a->b, a) och ger b
-- Uncurry tar in ($), och då blir argumenterna en tuppel (a->b, a) och som ger b
uncurry ($) :: (a->b, a) -> b
-- takes a tuple of a function and an argument and returns the result of the function applied on the argument

-- uncurry (:)
uncurry :: (a -> b -> c) -> (a, b) -> c
(:) :: a -> [a] -> [a]
uncurry (:) :: (a, [a]) -> [a]
-- takes a pair of elem::a, and list::a and returns a with elem inserted at the front

-- uncurry (.)
uncurry :: (a -> b -> c) -> (a, b) -> c
(.) :: (b -> c) -> (a -> b) -> a -> c
uncurry (.) :: (b->c, a->b) -> a -> c
--takes a pair of functions and returns their composition

-- uncurry uncurry
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry uncurry :: (a -> b -> c, (a, b)) -> c
-- takes a pair of a 2-arg function in curried form, and a pair of arguments, and returns the result of applying, the function on the first and second elem of the pair.

-- curry uncurry
curry   :: ((a, b) -> c) {-expects a tuple as argument-} -> a -> b -> c
uncurry :: (a -> b -> c) -> (a, b) -> c {-returns a result as argument-}
curry uncurry :: -- TYPE ERROR


-- 6
{-
6. Given the following three lists of tuples:

(personnummer, (efternamn, förnamn)):

pn :: [ (String, (String,String)) ]

(personnummer, sTiL-ID):

ps :: [ (String, String) ]

(sTiL-ID, epost):

se :: [ (String, String) ]

write a function that generates a list of pairs of the format

(name, epost)

ne :: [ (String, String) ]

where name is a string of the form "förnamn efternamn". You may assume that the ps list contains all personal numbers from pn, while se contains all the StiL-IDs from ps.
Document your function with a nice signature.

-}

pn :: [ (String, (String,String)) ]
ps :: [ (String, String) ]
se :: [ (String, String) ]

ne :: [ (String, String) ]

--lookup searches in a map

getName :: (String, String) -> String 
getName (f,e) = f ++ " " ++ e 

findAll pn ps se = map find pn
	where find (p,n) = (getName n, fromJust $ lookup (fromJust $ lookup p ps) se)

