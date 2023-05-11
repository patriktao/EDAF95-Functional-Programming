{-
Patrik Tao pa0676ta-s
Andreas Ruggieri an1775ru-s

Write these commands:
1. stack ghci
2. :load Assignment1.hs
3. main
4. write the name of the test file. e.g., easy50.txt
 -}

module Sudoku where

import Data.List

type Board = [(String, Int)]

type Sudoku = String

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x, y] | x <- xs, y <- ys]

squares :: String -> [String]
squares rows = cross rows $ take (length rows) ['1' ..]

unitList :: String -> [[String]]
unitList rowString = rows' ++ cols' ++ boxes rows'
  where
    len = (round . sqrt . fromIntegral) (length rowString)
    rows' = chunks (length rowString) $ squares rowString -- ABCD och 1234 -> [A1 A2 A3 A4] osv...
    cols' = transpose rows' -- Transposing rows' turn into -> [A1 B1 C1 D1] osv...
    halves = transpose . map (chunks len) -- [A1 A2] [B1 B2]|| help function for boxes: .map (chunks len ) applies chunks len to each inner array of the input array
    boxes rows = chunks (length rowString) $ concat $ concat $ halves rows -- [A1 A2 B1 B2]

filterUnitList :: String -> String -> [[String]]
filterUnitList square rows = filter (elem square) $ unitList rows

units :: String -> [(String, [[String]])]
units rows = [(square, filterUnitList square rows) | square <- squares rows]

foldList :: [[a]] -> [a]
foldList = concat

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

peers :: String -> [(String, [String])]
peers rows = [(sqr, filter (/= sqr) (removeDuplicates (foldList adjacents))) | (sqr, adjacents) <- units rows]

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x -- fråga

getPeers :: String -> String -> [String]
getPeers input rows = fromMaybe [] (lookup input $ peers rows)

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList xs = [x | Just x <- xs]

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups input_list list = justifyList [lookup x list | x <- input_list]

validSquare :: (String, Int) -> Board -> String -> Bool
validSquare (v, 0) list rows = True
validSquare (s, i) board rows = notElem i $ lookups (getPeers s rows) board

-- FEL, använd validUnit
-- få ut alla units tillhörande squaren
-- evaluera ifall är oblockerande
validBoard :: Board -> String -> Bool
validBoard board rows = all (\tuple -> validSquare tuple board rows) board

validUnit :: String -> [String] -> [(String, [Int])] -> Bool
validUnit _ [] board = False
validUnit rows unit board = all (\n -> elem n $ concat (lookups unit board)) nums
  where
    nums = [1 .. length rows]

reduceList :: Eq a => [a] -> [a] -> [a]
reduceList = foldl (\list_a b -> filter (/= b) list_a)

validSquareNumbers :: String -> (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers rows (s, i) list
  -- \| elem i possiblePairs = (s, [])
  | i `elem` possiblePairs = (s, [])
  | i /= 0 = (s, [i])
  | otherwise = (s, reduceList inf possiblePairs)
  where
    possiblePairs = filter (/= 0) $ lookups (getPeers s rows) list
    inf = [1 .. length rows]

validBoardNumbers :: String -> Board -> [(String, [Int])]
validBoardNumbers rows board = map (\tuple -> validSquareNumbers rows tuple board) board

validUnits :: String -> Board -> Bool
validUnits rows board = all (\unit -> validUnit rows unit $ validBoardNumbers rows board) (unitList rows)

sudokuSize :: [(String, Int)] -> Int
sudokuSize = round . sqrt . fromIntegral . length

sudokuStringSize :: String -> Int
sudokuStringSize = round . sqrt . fromIntegral . length

printSudoku :: [(String, Int)] -> String -> IO ()
printSudoku cells rowString = mapM_ print rows'
  where
    showSq :: (String, Int) -> String
    showSq sq@(string, val) = if validSquare sq cells rowString then show val else "x"
    showRows = map showSq cells
    chunkSize = (round . sqrt . fromIntegral . length) cells
    rows' = chunks chunkSize showRows

parseBoard :: String -> String -> Board
parseBoard board rows = zip (squares rows) (map charToInt board)

verifySudoku :: String -> String -> Bool
verifySudoku board rows = validUnits rows $ parseBoard board rows

{- Self Implemented Functions -}
charToInt :: Char -> Int
charToInt c = fromEnum c - fromEnum '0'

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter list = splitOn' delimiter list []
  where
    splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
    splitOn' _ [] acc = [acc]
    splitOn' delimiter (x : xs) acc
      | take (length delimiter) (x : xs) == delimiter = acc : splitOn' delimiter (drop (length delimiter) (x : xs)) []
      | otherwise = splitOn' delimiter xs (acc ++ [x])

{- Main Functions -}
main :: IO ()
main = do
  putStr "Solve Sudoku, enter filename: "
  i <- getLine
  s <- readFile i
  let numbers = filter (/= "") $ splitOn "=" $ concat $ lines s
  mapM_ checkSudoku numbers
  where
    checkSudoku board = do
      let size = sudokuStringSize board
      let rowString = take size ['A' ..]
      print $ verifySudoku board rowString
      printSudoku (parseBoard board rowString) rowString
      putStrLn " "