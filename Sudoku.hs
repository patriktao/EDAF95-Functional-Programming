-- Patrik Tao pa0676ta-s
-- Andreas Ruggieri an1775ru-s

module Sudoku where

import Data.Char (digitToInt)
import Data.List
import Data.List.Split
import Data.Sequence (chunksOf)
import System.Random

rows :: String
rows = "ABCDEFGHI"

cols :: String
cols = "123456789"

type Board = [(String, Int)]

type Sudoku = String

-- makes chunks of an array of specified length
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs

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
-- zip creates a list of tuples, with elements of first array mapped to the elements of second array
parseBoard :: String -> Board
parseBoard = zip squares . map digitToInt . replacePointsWithZeros

-- PART 2: SUDOKU PROBLEM

-- task 1, calculate a value unitList of all possible units (rows, cols, boxes)
unitList :: [[String]]
unitList = rows' ++ cols' ++ boxes rows'
  where
    rowLength = length rows
    len = (round . sqrt . fromIntegral) rowLength
    rows' = chunks rowLength squares -- ABCD och 1234 -> [A1 A2 A3 A4] osv...
    cols' = transpose rows' -- Transposing rows' turn into -> [A1 B1 C1 D1] osv...
    halves = transpose . map (chunks len) -- [A1 A2] [B1 B2]|| help function for boxes: .map (chunks len ) applies chunks len to each inner array of the input array
    boxes = chunks rowLength . concat . concat . halves -- [A1 A2 B1 B2] || We send in rows' into this function (it's abstractified due to point-free solution)

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
validBoard board = all (`validSquare` board) board

-- validBoard board = False `notElem` map (`validSquare` board) board
-- validBoard board = all (\sqr -> validSquare sqr board) board

{-
TIPS----
flip gör en funktion som tar in två argumenter och spottar ut en ny funtion med omvända parametrar
point-free se till att en annan funktion hanterar parametrarna
\$ -> ()
\c -> definerar en arrow funktion, därefter kan man göra en (\c -> validSquare c xs) för att specifera exakt vad som ska göras i funktionen.
-}

-- task 3 (nerflyttad)
-- verifySudoku :: Sudoku -> Bool
-- verifySudoku str = validBoard $ parseBoard str
-- verifySudoku = validBoard . parseBoard

-- Part 4
-- task 1
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList = foldl (\list_a b -> filter (/= b) list_a)

-- reduceList list_a [] = list_a
-- reduceList list_a (b:list_b)= reduceList (filter (/= b) list_a) list_b

-- part 4, task 2
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (s, i) list
  -- \| elem i possiblePairs = (s, [])
  | i `elem` possiblePairs = (s, [])
  | i /= 0 = (s, [i])
  | otherwise = (s, reduceList inf possiblePairs)
  where
    possiblePairs = filter (/= 0) $ lookups (getPeers s) list
    inf = [1 .. length rows]

-- returnerar sig själv om den är valid
-- returnerar tom lista så är den fel

-- part 4, task 3
validBoardNumbers :: Board -> [(String, [Int])]
validBoardNumbers board = map (`validSquareNumbers` board) board

-- validBoardNumbers board = map (\sqr -> validSquareNumbers sqr board) board

-- part 4, task 4
-- checka för att se om en unit är valid
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit [] board = False
validUnit unit board = all (\n -> elem n $ concat (lookups unit board)) nums
  where
    nums = [1 .. length rows]

-- validUnit list board = concat `notElem` lookups (getPeers list) board
-- validUnit unit board = and (map (\n -> elem n $ concat (lookups unit board)) nums)

validUnits :: Board -> Bool
validUnits board = all (\n -> validUnit n $ validBoardNumbers board) unitList

verifySudoku :: String -> Bool
verifySudoku string = validUnits $ parseBoard string

-- verifySudoku = validBoard . parseBoard

-- Lab 3 föreberedelser
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
-- intercalate unfolds an array
printSudoku1 :: [(String, Int)] -> IO ()
printSudoku1 cells = do
  mapM_ printRow rows
  where
    printRow row =
      do
        let rowCells = map snd $ filter (\(name, _) -> head name == row) (sort cells)
        putStrLn $ " " ++ intercalate "  " (map show rowCells) ++ " "

-- Lab 3
-- task 1
-- indicate in sudoku where there is error
printSudoku :: [(String, Int)] -> IO ()
printSudoku cells = mapM_ print rows'
  where
    showSq :: (String, Int) -> String
    showSq sq@(string, val) = if validSquare sq cells then show val else "x"
    showRows = map showSq cells
    chunkSize = (round . sqrt . fromIntegral . length) cells
    -- chunkSize = round $ sqrt $ fromIntegral $ length cells
    rows' = chunks chunkSize showRows

{- splitOn :: String -> String -> [String]
splitOn delim "" = []
splitOn delim s =
  let word = takeWhile (/= delim) $ dropWhile (== delim) s
      (_, rest) = splitAt (length word) s
   in word : splitOn (dropWhile (== delim) rest) -}

main :: IO ()
main =
  do
    s <- readFile "easy50.txt"
    let numbers = splitOn "========" $ concat $ lines s
    mapM_ checkSudoku numbers
  where
    checkSudoku b =
      do
        print $ verifySudoku b
        printSudoku $ parseBoard b
        putStrLn " "