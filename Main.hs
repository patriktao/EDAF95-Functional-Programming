-- Andreas Ruggieri an1775ru-s


-- module Sudoku where

import Data.Char (digitToInt)
-- import Data.list
import Data.List.Split (splitOn)
import System.Random

type Board = [(String, Int)]
type Sudoku = String

rows = "ABCD"

cols = "1234"

-- checks if list contains an element
containsElem :: Eq a => a -> [a] -> Bool
containsElem = elem
-- containsElem _ [] = False
-- containsElem elem (x : xs)
--  | elem == x = True
--  | otherwise = containsElem elem xs

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

digToInt :: Char -> Int
digToInt '0' = 0
digToInt '1' = 1
digToInt '2' = 2
digToInt '3' = 3
digToInt '4' = 4
digToInt '5' = 5
digToInt '6' = 6
digToInt '7' = 7
digToInt '8' = 8
digToInt '9' = 9

-- Write a function parseBoard which takes a board string as input and returns a list of tuples representing the board as mentioned above.
parseBoard :: String -> [(String, Int)]
parseBoard = zip squares . map digToInt . replacePointsWithZeros

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
getPeers input = fromMaybe [""] (lookup input peers)


--  getPeers = fromMaybe [""] . flip lookup peers

-- task 4, ta ut alla Just element från maybe lista
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList xs = [x | Just x <- xs]

-- task 5, tar in en lista av input values och gör lookups
lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups input list = justifyList [lookup x list | x <- input]

-- Part 3 : Sudoku Verifier, simple conflicts


-- task 1, Tar och kollar om en given ruta uppfyller sudokus regler för det givna brädet
validSquare :: (String, Int) -> Board -> Bool
validSquare input@(v, 0) list = True
validSquare (s, v) list = not $ containsElem v (lookups (getPeers (s)) list)
--validSquare (s, v) list = v `notElem` lookups (getPeers s) list






-- task 2,  Kollar om alla rutor i ett sudokubräde är giltiga
validBoard :: Board -> Bool
validBoard xs = all  (`validSquare` xs) xs
-- fråga varför 2 xs
-- validBoard xs = all (==True) (map (\c -> validSquare c xs) xs)
-- \c -> definierar en arrow funktion, där man kan gör en (map (\c -> validSquare c xs) för att specificera
-- exakt vad som ska göras i funktionen
-- gå igenom alla tuplar och utför validSquare på dom. Om alla returnerar true så är det true, annars False

-- task 3,  
-- verifySudoku :: Sudoku -> Bool
-- verifySudoku = validBoard . parseBoard
--verifySudoku str = length str == 16 && validBoard (parseBoard str)
-- skriv in att length behöver vara 16


-- Part 4: Sudoku verifier, blocking conflicts

-- Task 1

reduceList :: Eq a => [a] -> [a] -> [a]
reduceList xs [] = xs
reduceList xs (y:ys) = reduceList (filter (/= y) xs) ys
-- reduceList = foldl (\ list_a n -> filter (/= b) list_a)

-- Task 2
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
-- validSquareNumbers input@(s, v) list = if (validSquare input list) then (s, [v]) else (s, [])
validSquareNumbers input@(s, v) list 
 | v /= 0 && val = (s, [v]) 
 | v /= 0 && not (val) = (s, [])
 | otherwise = (s, reduceList inf possiblePeers)
    where 
      possiblePeers = filter (/= 0) $ lookups (getPeers s) list
      inf = [1, 2, 3, 4]
      val = not $ containsElem v (lookups (getPeers (s)) list)

-- Task 3 

validBoardNumbers :: Board -> [(String, [Int])]
validBoardNumbers board = map (\sqr -> validSquareNumbers sqr board) board

-- validBoardNumbers board = map (`validSquareNumbers` board) board

-- Task 4

-- checka för att se om en unit är valid

nums :: [Int]
nums = [1, 2, 3, 4]

validUnit :: [String] -> [(String, [Int])] -> Bool 
validUnit [] board = False
validUnit unit board = all (\n -> elem n $ concat (lookups unit board)) nums


-- validUnit list board = concat `notElem` lookups (getPeers list) board
-- validUnit unit board = and (map (\n -> elem n $ concat (lookups unit board)) nums)

-- Task 5

validUnits :: Board -> Bool
validUnits board = all (\n -> validUnit n $ validBoardNumbers board) unitList

-- Task 6
verifySudoku :: Sudoku -> Bool
verifySudoku string = validUnits $ parseBoard string
-- verifySudoku = validBoard . parseBoard
--verifySudoku str = length str == 16 && validBoard (parseBoard str)

-- Labb 3, Förberedelser
-- Task 1
-- Reads two integer values from input, and prints a random number in between those values
-- show :: Show a => a -> String

giveMeANumber :: IO ()
giveMeANumber = do
  putStrLn "Enter the lower bound:"
  lowerBound <- readLn :: IO Int
  putStrLn "Enter the upper bound:"
  upperBound <- readLn :: IO Int
  putStrLn $ "Your random number is: " ++ show upperBound
  randomNumber <- randomRIO (lowerBound, upperBound)
  putStrLn $ "Your random number is: " ++ show randomNumber

 -- giveMeANumber :: IO ()
 -- giveMeANumber = do
 -- putStrLn "Enter the lower bound:"
 -- lowerInput <- getLine
 -- let lowerBound = read lowerInput :: Int
 -- putStrLn "Enter the upper bound:"
 -- upperInput <- getLine
 -- let upperBound = read upperInput :: Int
 -- rand <- randomRIO (lowerBound, upperBound) :: IO Int
 -- print rand


-- task 2
-- ReadMaybe
-- readMaybe is a function that attempts to parse a string into a value of a specified type using the Read type class. The function returns Nothing if the parsing fails, or Just the parsed value if the parsing succeeds.

-- Task 3
-- show :: Show a => a -> String
-- mapM_ används för att loopa igenom en array som utsätts för monadic actions
-- printSudoku :: [(String, Int)] -> IO ()
-- printSudoku cells = do
-- intercalate unfolds an array

-- printSudoku1 :: [(String, Int)] -> IO ()
--printSudoku1 cells = do
--  mapM_ printRow rows
--  where
--    printRow row =
--      do
--        let rowCells = map snd $ filter (\(name, _) -> head name == row) (sort cells)
--       putStrLn $ " " ++ intercalate "  " (map show rowCells) ++ " "




-- traverse through cells and find the tuple, where the first element equals square
-- print the second element of the tuple

-- if val == 0
--  then putStrLn " "
--  else putStrLn $ show val
-- [("A4", 1), ("B3", 0)]
-- Lab Assignment: Reading and Writing Sudoku

-- Task 1

-- indicate in sudoku where there is error
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs


-- A1 A2 A3 A4
-- B1 B2 B3 B4
-- C1 C2 C3 C4
-- D1 D2 D3 D4


printSudoku :: [(String, Int)] -> IO ()
printSudoku cells = mapM_ print rows'
  where
    showSq :: (String, Int) -> String
    showSq sq@(string, val) = if validSquare sq cells then show val else "x"
    showRows = map showSq cells
    chunkSize = round $ sqrt $ fromIntegral $ length cells
    rows' = chunks chunkSize showRows


-- Task 2


splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter list = splitOn' delimiter list []
  where
    splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
    splitOn' _ [] acc = [acc]
    splitOn' delimiter (x : xs) acc
      | take (length delimiter) (x : xs) == delimiter = acc : splitOn' delimiter (drop (length delimiter) (x : xs)) []
      | otherwise = splitOn' delimiter xs (acc ++ [x])



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


-- Labb 4, Förberedelser
-- Tar in en tuppel bestående av funktioner och en tuppel bestående av värden. 
-- Outputen är en tuppel av resultatet när funktionerna exekveras på värdena.
map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f, g) (x, y) = (f x, g y)

-- tar in två funktioner och en lista. Den första funktionen utförs på ett element om den andra funktionen ger värdet true 
-- när den utförs på ett element
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf f p xs = [if p x then f x else x | x <- xs]
-- mapIf f p xs = map (\x -> if p x then f x else x) xs

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just x) _ = Just x
maybeOr _ (Just y) = Just y
maybeOr _ _ = Nothing

-- Takes a list of 'Maybe' values and return the first 'Just' value encountered or 'Nothing' if none of the values are 'Just'
firstJust :: [Maybe a] -> Maybe a
firstJust []           = Nothing
firstJust (Just x : _) = Just x
firstJust (_ : xs)     = firstJust xs

-- Takes a key of type a and a list of pairs [(a, [b])]
-- The function recursively searches for the key in each pair until a match is found or the list is exhausted.
lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList key ((k, values) : rest)
  | key == k = values
  | otherwise = lookupList key rest

-- Part 1: Binding your knowledge with bind

-- Task 1

-- Tar in en maybe variabel, om variabeln är Just returnas funktionen applicerad på det värdet
-- Om värdet på variabeln är Nothing returneras Nothing

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just a) f = f a

-- Task 2
tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x:xs)
  | x == y = Just (y':xs)
  | otherwise = fmap (x:) $ tryReplace y y' xs

-- Task 3
--recursiveReplacement :: [a] -> [a] -> [a] -> Maybe [a]
 

-- Part 2: The assign function

-- Task 1
-- The first function sets an Int value to the string square in the Board and the
-- eliminateValue does the same for elimination.

-- Use the functions mapIf and map2 you have prevously implemented

-- Kolla om String finns i Boarden, isåfall ändra värdet på den platsen
setValue, eliminateValue :: Int -> String -> Board -> Board
setValue x str board = mapIf (map2 (id, updateValue)) matches board
  where
    updateValue i = if i == str then x else i
    matches (s, _) = s == str












 



        


