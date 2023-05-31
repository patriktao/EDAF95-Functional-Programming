-- Patrik Tao
-- Andreas Ruggieri

{- 
1. ghci
2. :load Assign2.hs 
3. main 
4. type your name of the file, e.g., easy50.txt 
5. follow the instructions
 -}


module SolveSudoku where

-- by Adrian Roth

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import System.Exit
import Text.Read (readMaybe)

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

-- Tar in en tuppel bestående av funktioner och en tuppel bestående av värden.
-- Outputen är en tuppel av resultatet när funktionerna exekveras på värdena.
map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f, g) (x, y) = (f x, g y)

-- tar in två funktioner och en lista. Den första funktionen utförs på ett element om den andra funktionen ger värdet true
-- när den utförs på ett element
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf f p = map (\x -> if p x then f x else x)

-- mapIf f p xs = [if p x then f x else x | x <- xs]

{- Returns just if one of the values are Just -}
maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr x y
  | isJust x = x
  | isJust y = y
  | otherwise = Nothing

{- Takes a list of 'Maybe' values and return the first 'Just' value encountered or 'Nothing' if none of the values are 'Just' -}
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (_ : xs) = firstJust xs

lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList key ((k, values) : rest)
  | key == k = values
  | otherwise = lookupList key rest

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just a) f = f a

tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x : xs)
  | x == y = Just (y' : xs)
  | otherwise = (x :) <$> tryReplace y y' xs

recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement [] _ _ = Just []
recursiveReplacement (x : xs) (y : ys) list = tryReplace x y list >>= recursiveReplacement xs ys

setValue, eliminateValue :: Int -> String -> Board -> Board
setValue val sqr = mapIf (map2 (id, const [val])) ((== sqr) . fst)
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

assign :: Int -> String -> Board -> Maybe Board
assign _ _ [] = Nothing
assign val sqr board = do
  let allSquarePeers = getPeers sqr
  maybeBoard <- assign' val allSquarePeers board
  Just $ setValue val sqr maybeBoard

{- Vi går igenom varje peer rekursivt och tar bort alla möjliga "value"-}
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

{- Labb 1 Funktioner -}
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

parseBoardToPrint :: String -> [(String, Int)]
parseBoardToPrint board = zip squares (map charToInt board)

sudokuStringSize :: String -> Int
sudokuStringSize = round . sqrt . fromIntegral . length

printSudoku :: [(String, Int)] -> IO ()
printSudoku cells = mapM_ print rows'
  where
    showSq :: (String, Int) -> String
    showSq sq@(string, val)
      | validSquare (string, val) cells = if val == 0 then "_" else show val
    showRows = map showSq cells
    chunkSize = (round . sqrt . fromIntegral . length) cells
    rows' = chunks chunkSize showRows

charToInt :: Char -> Int
charToInt c = fromEnum c - fromEnum '0'

validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (v, 0) list = True
validSquare (s, i) board = notElem i $ lookups (getPeers s) board

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups input_list list = justifyList [lookup x list | x <- input_list]

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList xs = catMaybes xs

{- Assignment 2 Functions -}
main :: IO ()
main = do
  putStr "\n"
  putStrLn "-----------------------------------------------"
  putStrLn "Welcome to Sudoku Solver by Andreas and Patrik"
  putStr "Enter your filename: "
  i <- getLine
  s <- readFile i
  let boards = filter (/= "") $ splitOn "=" $ concat $ lines s
  let currBoard = head boards
  putStr "\n"
  putStrLn "Here is the first sudoku read from the file:"
  chooseAlternative boards currBoard

chooseAlternative :: [String] -> String -> IO ()
chooseAlternative boards currBoard = do
  putStr "\n"
  putStrLn "-----------------------------------------------"
  putStrLn "Current Board:"
  printSudoku (parseBoardToPrint currBoard)
  putStr "\n"
  putStrLn "Choose one of the alternatives"
  putStrLn "1. Solve the current sudoku and show the result"
  putStrLn "2. Solve all the sudokus in the file"
  putStrLn "3. Choose a square and assign a value to it"
  putStrLn "4. Quit"
  putStr "\n"
  putStr "What do you want to do?: "
  input <- getLine
  if input >= show 1 && input <= show 4
    then do
      let choice = read input :: Int
      case choice of
        1 -> solveCurrentSudoku boards currBoard -- solveSudoku -- call with the string
        2 -> solveAllSudokus boards -- solve all sudokus
        3 -> assignNumber boards currBoard -- assign
        4 -> do
          putStrLn "Bye bye!"
          exitSuccess
    else do
      putStr "Not a valid input, type a number between (1-4)"
      chooseAlternative boards currBoard

solveAllSudokus :: [String] -> IO ()
solveAllSudokus boards = mapM_ checkSudoku boards
  where
    checkSudoku board = do
      putStr "\n"
      (printSudoku . formatPrettyMaybeBoard . solveSudoku) board

solveCurrentSudoku :: [String] -> String -> IO ()
solveCurrentSudoku boards currBoard = do
  let solvedBoard = (stringArrayToString . formatPrettyMaybeBoard . solveSudoku) currBoard
  chooseAlternative boards solvedBoard

formatPrettyMaybeBoard :: Maybe Board -> [(String, Int)]
formatPrettyMaybeBoard board = case board of
  Just board -> map (\(id, intArray) -> if length intArray == 1 then (id, head intArray) else (id, 0)) board
  {- Problemet med denna lösning är när vi tilldelar en value till en viss square med funktionen assign, kommer detta värde att rekursivt elimineras från alla peers till denna square
    vilket medför att de peers som bara har 1 element kvar efter eliminationen visas upp i boarden  -}
  Nothing -> []

assignNumber :: [String] -> String -> IO ()
assignNumber boards currBoard = do
  putStr "Choose a square that you want to assign a value to: "
  sqr <- getLine
  if sqr `notElem` squares
    then do
      putStrLn "Not a valid square"
      assignNumber boards currBoard
    else askForNumber boards currBoard sqr

stringArrayToString :: [(String, Int)] -> String
stringArrayToString = concatMap (show . snd)

askForNumber :: [String] -> String -> String -> IO ()
askForNumber boards currBoard sqr = do
  putStr "Enter a number to put into the square: "
  input <- getLine
  case readMaybe input of
    Just number ->
      if number >= 0 && number <= 9
        then do
          case parseBoard currBoard of
            Just parsedBoard -> do
              let updatedBoard = assign number sqr parsedBoard
              case updatedBoard of
                Just newBoard -> do
                  putStrLn "\n"
                  putStrLn $ "Good job! Number: " ++ show number ++ " is now assigned to Square: " ++ sqr
                  chooseAlternative boards $ (stringArrayToString . formatPrettyMaybeBoard) updatedBoard
                Nothing -> do
                  putStrLn "\n"
                  putStrLn "Nice try! The number can't be assigned, try something else."
                  chooseAlternative boards currBoard
            Nothing -> do
              putStrLn "Error when parsing board. Please try again."
              chooseAlternative boards currBoard
        else do
          putStrLn "Please enter a number between 0 and 9."
          askForNumber boards currBoard sqr
    Nothing -> do
      putStrLn "Please enter a valid number."
      askForNumber boards currBoard sqr



-- Case -> pattern matching, matchar resultat till förväntat resultat
-- Guards -> Boolean, evaluerar argumenten