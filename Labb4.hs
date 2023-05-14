module SolveSudoku where

-- by Adrian Roth

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Distribution.Simple.Utils (xargs)
import GHC.Stack.CCS (whereFrom)

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

rows1 :: [(String, Integer)]
rows1 = [("A1", 0), ("A2", 0), ("A3", 0), ("A4", 0), ("A5", 0), ("A6", 0), ("A7", 0), ("A8", 0), ("A9", 0), ("B1", 0), ("B2", 0), ("B3", 0), ("B4", 0), ("B5", 0), ("B6", 0), ("B7", 0), ("B8", 0), ("B9", 0), ("C1", 0), ("C2", 0), ("C3", 0), ("C4", 0), ("C5", 0), ("C6", 0), ("C7", 0), ("C8", 0), ("C9", 0), ("D1", 0), ("D2", 0), ("D3", 0), ("D4", 0), ("D5", 0), ("D6", 0), ("D7", 0), ("D8", 0), ("D9", 0), ("E1", 0), ("E2", 0), ("E3", 0), ("E4", 0), ("E5", 0), ("E6", 0), ("E7", 0), ("E8", 0), ("E9", 0), ("F1", 0), ("F2", 0), ("F3", 0), ("F4", 0), ("F5", 0), ("F6", 0), ("F7", 0), ("F8", 0), ("F9", 0), ("G1", 0), ("G2", 0), ("G3", 0), ("G4", 0), ("G5", 0), ("G6", 0), ("G7", 0), ("G8", 0), ("G9", 0), ("H1", 0), ("H2", 0), ("H3", 0), ("H4", 0), ("H5", 0), ("H6", 0), ("H7", 0), ("H8", 0), ("H9", 0), ("I1", 0), ("I2", 0), ("I3", 0), ("I4", 0), ("I5", 0), ("I6", 0), ("I7", 0), ("I8", 0), ("I9", 0)]

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
  | null possibleValues = Nothing
  | val `notElem` possibleValues = Just board
  | null remainingValues = Nothing
  | length possibleValues == 1 = Nothing
  | otherwise = Just $ eliminateValue val sqr board
  where
    possibleValues = lookupList sqr board
    remainingValues = possibleValues \\ [val]

{- När vi assignar ett nytt värde på en square vill vi se till att det går att assigna värdet dvs. vi eliminerar de värdena på de ställen som är peers till squaren och sedan assignar vårt värde till den tänkta squaren.
Tanken är att i varje peer kunna ta bort value från [int] i deras tuppel (sqr, [int]) -}

assign :: Int -> String -> Board -> Maybe Board
assign _ _ [] = Nothing
assign val sqr board = do
  let allSquarePeers = getPeers sqr
  maybeBoard <- assign' val allSquarePeers board
  Just $ setValue val sqr maybeBoard

{- Vi går igenom varje peer rekursivt och tar bort alla möjliga "value" -}
assign' :: Int -> [String] -> Board -> Maybe Board
assign' val (x : xs) board
  | null board = Just []
  | null xs = eliminateFromBoard
  | otherwise = eliminateFromBoard >>= assign' val xs
  where
    eliminateFromBoard = eliminate val x board

getPeers :: String -> [String]
getPeers sqr = case filter (\(s, _) -> s == sqr) peers of
  [(_, ys)] -> ys
  _ -> []

solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] board = Just board
solveSudoku' (sqr : squares) board = firstJust $ map (\val -> assign val sqr board >>= solveSudoku' squares) possibleValues
  where
    possibleValues = lookupList sqr board

solveSudoku :: String -> Maybe Board
solveSudoku board = parseBoard board >>= solveSudoku' squares