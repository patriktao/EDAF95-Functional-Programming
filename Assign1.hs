-- Patrik Tao pa0676ta-s
-- Andreas Ruggieri an1775ru-s

module Sudoku where

import Data.List
import Data.List.Split

type Board = [(String, Int)]

type Sudoku = String

containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x : xs)
  | elem == x = True
  | otherwise = containsElem elem xs

cross :: [a] -> [a] -> [[a]] -- declare input output
cross xs ys = [[x, y] | x <- xs, y <- ys] -- expected behaviour

squares :: String -> [String]
squares rows = cross rows $ take (length rows) ['1' ..]

unitList :: String -> [[String]]
unitList rowString = rows' ++ cols' ++ boxes rows'
  where
    rowLength = length rowString
    len = (round . sqrt . fromIntegral) rowLength
    rows' = chunks rowLength $ squares rowString -- ABCD och 1234 -> [A1 A2 A3 A4] osv...
    cols' = transpose rows' -- Transposing rows' turn into -> [A1 B1 C1 D1] osv...
    halves = transpose . map (chunks len) -- [A1 A2] [B1 B2]|| help function for boxes: .map (chunks len ) applies chunks len to each inner array of the input array
    boxes rows = chunks rowLength $ concat $ concat $ halves rows -- [A1 A2 B1 B2]

filterUnitList :: String -> String -> [[String]]
filterUnitList square rows = filter (containsElem square) $ unitList rows

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

{- Valid Square -}
validSquare :: (String, Int) -> Board -> String -> Bool
validSquare (v, 0) list rows = True
validSquare (s, i) board rows = notElem i $ lookups (getPeers s rows) board

validBoard :: Board -> String -> Bool
validBoard board rows = all (\sqr -> validSquare sqr board rows) board

verifySudoku :: String -> String -> Bool
verifySudoku board rows = validBoard (parseBoard board rows) rows

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

charToInt :: Char -> Int
charToInt c = fromEnum c - fromEnum '0'

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs

main :: IO ()
main = do
  putStrLn "Solve Sudoku, enter filename"
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