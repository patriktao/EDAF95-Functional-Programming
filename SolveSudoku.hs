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
Outputen är en tuppel av resultatet när funktionerna exekveras på värderna
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
{- Checks whether the input value is Nothing or a value, if it's a value then execute the function -}
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just a) f = f a

{- >>= -}

{- This takes two values and a list of elements and possibly returns a list where the first occurnce of the element has been replaced. What it does is that it checks whether we can replace an element and whether
 -}
tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x : xs) -- y shall be replaced by y'
  | x == y = Just (y' : xs) -- appends the replacement x to list xs
  | otherwise = fmap (x :) $ tryReplace y y' xs -- appends x to the the returned list from the recursion, adding back the result.
  -- vi behöver dessutom fmap eftersom vi loopar på en lista Maybe [a] och vi vill köra Just på ett element i listan.

{- Rekursivt, vi vill att [1,2] ska ersättas med [3,4] i en lista [a] -}
{- Om tryReplace returnerar en Just vektor i maybeBind, skickas denna som tredje parametern i recursiveReplacement med xs och ys, och rekursionen fortrsätter -}
recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement [] _ _ = Just []
-- recursiveReplacement (x : xs) (y : ys) list = maybeBind (tryReplace x y list) (recursiveReplacement xs ys)
recursiveReplacement (x : xs) (y : ys) list = tryReplace x y list >>= recursiveReplacement xs ys

{- Ifall tryreplace returnerar en just, så körs recursiveReplacement med resultatet från den förra -}

-- Part 2
setValue :: Int -> String -> Board -> Board
setValue val sqr = mapIf (map2 (id, const [val])) ((== sqr) . fst)

-- setValue val sqr board = mapIf (map2 (id, const [val])) ((== sqr) . fst) board

eliminateValue :: Int -> String -> Board -> Board
eliminateValue val sqr = mapIf (map2 (id, filter (/= val))) ((== sqr) . fst)

eliminate :: Int -> String -> Board -> Maybe Board
eliminate val sqr board
  | length possibleValues - 1 == 0 = Nothing
  | length possibleValues == 1 = Nothing
  | val `notElem` possibleValues = Nothing
  | otherwise = Just $ eliminateValue val sqr board
  where
    possibleValues = lookupList sqr board

{- När vi assignar ett nytt värde på en square vill vi se till att det går att assigna värdet dvs. vi eliminerar de värdena på de ställen som är peers till squaren och sedan assignar vårt värde till den tänkta squaren.
Tanken är att i varje peer kunna ta bort value från [int] i deras tuppel (sqr, [int]) -}

assign :: Int -> String -> Board -> Maybe Board
assign _ _ [] = Nothing
assign val sqr board = do
  let allSquarePeers = getPeers sqr
  assign' val allSquarePeers board
  Just (setValue val sqr board)

{- Vi går igenom varje peer rekursivt och tar bort alla möjliga "value" -}
assign' :: Int -> [String] -> Board -> Maybe Board
assign' _ _ [] = Just []
assign' val (x : xs) board = eliminate val x board >>= assign' val xs

getPeers :: String -> [String]
getPeers sqr = case filter (\(s, _) -> s == sqr) peers of
  [(_, ys)] -> ys
  _ -> []
