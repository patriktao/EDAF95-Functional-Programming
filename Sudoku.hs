-- Patrik Tao pa0676ta-s
-- Andreas Ruggieri an1775ru-s

module Sudoku where

import Data.Char (digitToInt)
import Data.List
import System.Random

rows = "ABCD"

cols = "1234"

type Board = [(String, Int)]

type Sudoku = String

-- checks if list contains an element
containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x : xs)
  | elem == x = True
  | otherwise = containsElem elem xs

-- Calculates the cartesian product of two lists.
cross :: [a] -> [a] -> [[a]] -- declare input output
cross xs ys = [[x, y] | x <- xs, y <- ys] -- expected behaviour

-- Part 1: Sudoku Board
-- task 1
replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c == '.' then '0' else c)

-- task 2
squares :: [String]
squares = cross rows cols

-- task 3
-- Write a function parseBoard which takes a board string as input and returns a list of tuples representing the board as mentioned above.
parseBoard :: String -> Board
parseBoard = zip squares . map digitToInt . replacePointsWithZeros

-- PART 2: SUDOKU PROBLEM

-- task 1, calculate a value unitList of all possible units (rows, cols, boxes)
unitList :: [[String]]
unitList = boxes ++ rows' ++ cols'
  where
    boxes = [cross xs ys | xs <- ["AB", "CD"], ys <- ["12", "34"]]
    rows' = [cross xs ys | xs <- ["A", "B", "C", "D"], ys <- ["1234"]]
    cols' = [cross xs ys | xs <- ["ABCD"], ys <- ["1", "2", "3", "4"]]

-- unitList = rows' ++ transpose rows' ++ boxes rows'
--   where
--      len = (round . sqrt. fromIntegral . length) rows
--      rows' = chunksOf len cells
--      boxes = chunksOf len . concat . concat . halves
--      halves = transpose . map (chunkOf lem)

-- task 2
filterUnitList :: String -> [[String]]
filterUnitList square = filter (containsElem square) unitList

-- task 3
units :: [(String, [[String]])]
units = [(square, filterUnitList square) | square <- squares]

-- task 4
foldList :: [[a]] -> [a]
foldList = concat

-- task 5
-- eq is used to say that parameter a can be evaluated as equal or unequal
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

-- removeDuplicates: nub
-- (filter (/= x) xs) removes all elements from xs that are equal to x

-- task 6
peers :: [(String, [String])]
peers = [(sqr, filter (/= sqr) (removeDuplicates (foldList adjacents))) | (sqr, adjacents) <- units]

-- Labb 2, part 2

-- task 2
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x -- fråga
-- fromMaybe input x = case x of Nothing -> input; Just v -> v

-- task 3, ta ut alla möjliga peers från getPeers listan
getPeers :: String -> [String]
getPeers input = fromMaybe [] (lookup input peers)

--  getPeers = fromMaybe [""] . flip lookup peers

-- task 4, ta ut alla Just element från maybe lista
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList xs = [x | Just x <- xs]

-- task 5, tar in en lista av input values och gör lookups
lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups input_list list = justifyList [lookup x list | x <- input_list]

-- part 3
-- task 1 kollar om rutan uppfyller reglerna
validSquare :: (String, Int) -> Board -> Bool
validSquare (v, 0) list = True
validSquare (s, i) board = notElem i $ lookups (getPeers s) board

-- validSquare (s, i) list = i `notElem` lookups (getPeers s) list
-- elem s (lookups (getPeers v) list)

-- task 2
validBoard :: Board -> Bool
-- validBoard board = False `notElem` map (`validSquare` board) board

-- validBoard board = notElem False (map (\sqr -> validSquare sqr board) board)
validBoard board = all (`validSquare` board) board

{-
TIPS----
flip gör en funktion som tar in två argumenter och spottar ut en ny funtion med omvända parametrar
point-free se till att en annan funktion hanterar parametrarna
\$ -> ()
\c -> definerar en arrow funktion, därefter kan man göra en (\c -> validSquare c xs) för att specifera exakt vad som ska göras i funktionen.
-}

-- task 3
-- verifySudoku :: Sudoku -> Bool
-- verifySudoku str = validBoard $ parseBoard str
-- verifySudoku = validBoard . parseBoard

-- Part 4
-- task 1

reduceList :: Eq a => [a] -> [a] -> [a]
-- reduceList list_a [] = list_a
-- reduceList list_a (b:list_b)= reduceList (filter (/= b) list_a) list_b
reduceList = foldl (\list_a b -> filter (/= b) list_a)

-- part 4, task 2
--   | elem i possiblePairs = (s, [])
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (s, i) list
  | i `elem` possiblePairs = (s, [])
  | i /= 0 = (s, [i])
  | otherwise = (s, reduceList inf possiblePairs)
  where
    possiblePairs = filter (/= 0) $ lookups (getPeers s) list
    inf = [1, 2, 3, 4]

-- part 4, task 3
validBoardNumbers :: Board -> [(String, [Int])]
validBoardNumbers board = map (\sqr -> validSquareNumbers sqr board) board

-- validBoardNumbers board = map (`validSquareNumbers` board) board

-- part 4, task 4

-- checka för att se om en unit är valid
nums :: [Int]
nums = [1, 2, 3, 4]

validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit [] board = False
validUnit unit board = all (\n -> elem n $ concat (lookups unit board)) nums

-- validUnit list board = concat `notElem` lookups (getPeers list) board
-- validUnit unit board = and (map (\n -> elem n $ concat (lookups unit board)) nums)

validUnits :: Board -> Bool
validUnits board = all (\n -> validUnit n $ validBoardNumbers board) unitList

verifySudoku :: Sudoku -> Bool
verifySudoku string = validUnits $ parseBoard string

-- verifySudoku = validBoard . parseBoard

-- Lab 3
-- task 1
-- Reads two integer values from input, and prints a random number in between those values
-- show :: Show a => a -> String
giveMeANumber :: IO ()
giveMeANumber = do
  putStrLn "Enter the lower bound:"
  lowerInput <- getLine
  let lowerBound = read lowerInput :: Int
  putStrLn "Enter the upper bound:"
  upperInput <- getLine
  let upperBound = read upperInput :: Int
  rand <- randomRIO (lowerBound, upperBound) :: IO Int
  print rand

-- task 2
-- ReadMaybe
-- readMaybe is a function that attempts to parse a string into a value of a specified type using the Read type class. The function returns Nothing if the parsing fails, or Just the parsed value if the parsing succeeds.

-- task 3
-- show :: Show a => a -> String
-- mapM_ används för att loopa igenom en array som utsätts för monadic actions
printSudoku :: [(String, Int)] -> IO ()
printSudoku cells = do
  mapM_ printRow rows
  where
    printRow row =
      do
        let rowCells = map snd $ filter (\(name, _) -> head name == row) (sort cells)
        putStrLn $ " " ++ intercalate "  " (map show rowCells) ++ " "

-- traverse through cells and find the tuple, where the first element equals square
-- print the second element of the tuple

-- if val == 0
--  then putStrLn " "
--  else putStrLn $ show val

-- [("A4", 1), ("B3", 0)]

-- A1 A2 A3 A4
-- B1 B2 B3 B4
-- C1 C2 C3 C4
-- D1 D2 D3 D4