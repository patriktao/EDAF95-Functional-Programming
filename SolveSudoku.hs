module SolveSudoku where

-- by Adrian Roth

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Distribution.Simple.Utils (xargs)

cross :: [a] -> [a] -> [[a]]
cross s1 s2 = [[r, c] | r <- s1, c <- s2]

rowBoxes, colBoxes :: [String]
rowBoxes = ["ABC", "DEF", "GHI"]
colBoxes = ["123", "456", "789"]

rows, cols :: String
rows = concat rowBoxes
cols = concat colBoxes

squares :: [String]
squares = cross rows cols

unitlist :: [[String]]
unitlist =
  [cross rows [c] | c <- cols]
    ++ [cross [r] cols | r <- rows]
    ++ [cross rs cs | rs <- rowBoxes, cs <- colBoxes]

units :: [(String, [[String]])]
units = [(s, filter (elem s) unitlist) | s <- squares]

peers :: [(String, [String])]
peers = map (\(s, u) -> (s, delete s (foldl union [] u))) units

type Board = [(String, [Int])]

allDigits :: [Int]
allDigits = [1, 2, 3, 4, 5, 6, 7, 8, 9]

infAllDigits :: [[Int]]
infAllDigits = repeat allDigits

emptyBoard :: [(String, [Int])]
emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDigit x = assign (digitToInt x) s values
  | otherwise = fail "not a valid grid"

parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares

{- Labb 4 -}

{- Tar in en tuppel av funktioner och en tuppel bestående av två värden
Outputen är en tuppel av esultatet när funktionerna exekveras på värderna
 -}
map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f, g) (x, y) = (f x, g y)

{- Tar in två funktioner och en array.
Den första ekvationen exekveras ifall den andra funktioner ger true efter
att ha exekverat.
-}
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf f p xs = [if p x then f x else x | x <- xs]

{- Returns just if one of the values are Just -}
maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just x) _ = Just x
maybeOr _ (Just y) = Just y
maybeOr _ _ = Nothing

{- Takes a list of 'Maybe' values and return the first 'Just' value encountered or 'Nothing' if none of the values are 'Just' -}
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (_ : xs) = firstJust xs

{- Looks up all possible values b in a list where a is the key -}
lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList key list = concat [bs | (a, bs) <- list, a == key] -- List comprenhension

-- PART 1
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just a) f = f a

{- This takes two values and a list of elements and possibly returns a list where the first occurnce of the element has been replaced. What it does is that it checks whether we can replace an element and whether
 -}
tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x : xs) -- y shall be replaced by y'
  | x == y = Just (y' : xs) -- appends the replacement x to list xs
  | otherwise = fmap (x :) $ tryReplace y y' xs -- appends x to the the returned list from the recursion, adding back the result.
  -- vi behöver dessutom fmap eftersom vi loopar på en lista Maybe [a] och vi vill köra Just på ett element i listan.

{- infix `maybeBind`kör tryReplace på varje maybe element i listen [1,2,3] -}
doIt = ((Just [1, 2, 3] `maybeBind` tryReplace 1 3) `maybeBind` tryReplace 3 2) `maybeBind` tryReplace 2 1

{- Rekursivt, vi vill att [1,2] ska ersättas med [3,4] i en lista [a] -}
recursiveReplacement :: [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement [] _ _ = Just []
recursiveReplacement (x : xs) (y : ys) list = case tryReplace x (head y) list of
  Nothing -> Nothing
  Just list' -> fmap (x: ) % maybeBind (recursiveReplacement xs ys list') 
