module SolveSudoku where

-- by Adrian Roth

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
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
unitlist = [cross rows [c] | c <- cols]
        ++ [cross [r] cols | r <- rows]
        ++ [cross rs cs | rs <- rowBoxes, cs <- colBoxes]

units :: [(String, [[String]])]
units = [(s, filter (elem s) unitlist) | s <- squares]

peers :: [(String, [String])]
peers = map (\(s, u) -> (s, delete s (foldl union [] u))) units

type Board = [(String, [Int])]

allDigits :: [Int]
allDigits = [1, 2, 3, 4, 5, 6, 7, 8, 9]

infAllDigits = repeat allDigits
emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDigit x = assign (digitToInt x) s values
  | otherwise = fail "not a valid grid"

parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares

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

{- Returns just if one of the values are Just -}
maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just x) _ = Just x
maybeOr _ (Just y) = Just y
maybeOr _ _ = Nothing

{- Takes a list of 'Maybe' values and return the first 'Just' value encountered or 'Nothing' if none of the values are 'Just' -}
firstJust :: [Maybe a] -> Maybe a
firstJust []           = Nothing
firstJust (Just x : _) = Just x
firstJust (_ : xs)     = firstJust xs

-- Takes a key of type a and a list of pairs [(a, [b])]
-- The function recursively searches for the key in each pair until a match is found or the list is exhausted.
{- Looks up all possible values b in a list where a is the key -}
lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList key ((k, values) : rest)
  | key == k = values
  | otherwise = lookupList key rest
-- lookupList key list = concat [bs | (a, bs) <- list, a == key] -- List comprenhension

rows1 :: [(String, Integer)]
rows1 = [("A1", 0), ("A2", 0), ("A3", 0), ("A4", 0), ("A5", 0), ("A6", 0), ("A7", 0), ("A8", 0), ("A9", 0), ("B1", 0), ("B2", 0), ("B3", 0), ("B4", 0), ("B5", 0), ("B6", 0), ("B7", 0), ("B8", 0), ("B9", 0), ("C1", 0), ("C2", 0), ("C3", 0), ("C4", 0), ("C5", 0), ("C6", 0), ("C7", 0), ("C8", 0), ("C9", 0), ("D1", 0), ("D2", 0), ("D3", 0), ("D4", 0), ("D5", 0), ("D6", 0), ("D7", 0), ("D8", 0), ("D9", 0), ("E1", 0), ("E2", 0), ("E3", 0), ("E4", 0), ("E5", 0), ("E6", 0), ("E7", 0), ("E8", 0), ("E9", 0), ("F1", 0), ("F2", 0), ("F3", 0), ("F4", 0), ("F5", 0), ("F6", 0), ("F7", 0), ("F8", 0), ("F9", 0), ("G1", 0), ("G2", 0), ("G3", 0), ("G4", 0), ("G5", 0), ("G6", 0), ("G7", 0), ("G8", 0), ("G9", 0), ("H1", 0), ("H2", 0), ("H3", 0), ("H4", 0), ("H5", 0), ("H6", 0), ("H7", 0), ("H8", 0), ("H9", 0), ("I1", 0), ("I2", 0), ("I3", 0), ("I4", 0), ("I5", 0), ("I6", 0), ("I7", 0), ("I8", 0), ("I9", 0)]



-- Part 1: Binding your knowledge with bind

-- Task 1
-- Tar in en maybe variabel, om variabeln är Just returnas funktionen applicerad på det värdet
-- Om värdet på variabeln är Nothing returneras Nothing
{- Checks whether the input value is Nothing or a value, if it's a value then execute the function -}
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just a) f = f a

-- Task 2
{- This takes two values and a list of elements and possibly returns a list where the first occurnce of the element has been replaced. What it does is that it checks whether we can replace an element and whether
 -}
tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x:xs)
  | x == y = Just (y':xs)
  | otherwise = fmap (x:) $ tryReplace y y' xs

-- Task 3
{- Rekursivt, vi vill att [1,2] ska ersättas med [3,4] i en lista [a] -}
{- Om tryReplace returnerar en Just vektor i maybeBind, skickas denna som tredje parametern i recursiveReplacement med xs och ys, och rekursionen fortrsätter -}
recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement [] _ _ = Just []
-- recursiveReplacement (x : xs) (y : ys) list = maybeBind (tryReplace x y list) (recursiveReplacement xs ys)
recursiveReplacement (x : xs) (y : ys) list = tryReplace x y list >>= recursiveReplacement xs ys

{- Ifall tryreplace returnerar en just, så körs recursiveReplacement med resultatet från den förra -}

-- Part 2: The assign function

-- Task 1
-- The first function sets an Int value to the string square in the Board and the
-- eliminateValue does the same for elimination.

setValue, eliminateValue :: Int -> String -> Board -> Board
-- setValue val sqr board =  mapIf (map2 (id, const [val]) (sqr, allDigits)) (elem val lookupList sqr board) board
setValue val sqr = mapIf (map2 (id, const [val])) ((== sqr) . fst)
-- setValue val sqr board = mapIf (map2 (id, const [val])) ((== sqr) . fst) board

-- eliminateValue val sqr board = map2 (id, filter (/=val) ) (sqr, allDigits)
eliminateValue val sqr = mapIf (map2 (id, filter (/= val))) ((== sqr) . fst)

-- Task 2
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
 --   | length possibleValues == 1 = Nothing
 --   | val `notElem` possibleValues = Nothing
 --   | otherwise = Just $ eliminateValue val sqr board
 --   where
  --    possibleValues = lookupList sqr board


-- Task 3
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

-- assign' _ _ [] = Just []
-- assign' val ( x : xs) board = eliminate val x board ==> assign' val xs

getPeers :: String -> [String]
getPeers sqr = case filter (\(s, _) -> s == sqr) peers of
  [(_, ys)] -> ys
  _ -> []

solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] board = Just board
solveSudoku' (sqr : squares) board = firstJust $ map (\val -> assign val sqr board >>= solveSudoku' squares) possibleValues
  where
    possibleValues = lookupList sqr board
-- behöver förstå lambdaexpression

solveSudoku :: String -> Maybe Board
solveSudoku board = parseBoard board >>= solveSudoku' squares

printSudoku :: [(String, Int)] -> IO ()
printSudoku cells = mapM_ print rows'
  where
    showSq :: (String, Int) -> String
  --  showSq sq@(string, val) = if validSquare sq cells then show val else "x"
    showSq sq@(string, val) = if (1 == 1) then show val else "x"
    showRows = map showSq cells
    chunkSize = (round . sqrt . fromIntegral . length) cells
    rows' = chunks chunkSize showRows

--printOneSudoku :: [(String, Int)] -> IO ()
-- printOneSudoku cells 

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


parseBoard1 :: String -> [(String, Int)]
parseBoard1 board = zip (squares) (map charToInt board)      



main :: IO ()
main = do
  putStr "Solve Sudoku, enter filename: "
  i <- getLine
  s <- readFile i
  let numbers = filter (/= "") $ splitOn "=" $ concat $ lines s
  -- visa första sudokut bara, hur gör man? Take 1 (numbers) funkar ej
  chooseAlternative
  -- mapM_ checkSudoku numbers
  --where
   -- checkSudoku board = do
--      print $ verifySudoku board 
   --   printSudoku (parseBoard1 board)
   --   putStrLn " "

chooseAlternative :: IO ()
chooseAlternative = do
  putStrLn "Choose one of the alternatives"
  putStrLn "1. Solve the sudoku and show the result"
  putStrLn "2. Solve all the sudokus in the file"
  putStrLn "3. Choose a square and assign a value to it"
  input <- getLine
  let choice = read input :: Int
  case choice of 
    1 -> putStrLn "1" -- solveSudoku -- call with the string
    2 -> putStrLn "2" -- solve all sudokus
    3 -> num3 
  chooseAlternative 

-- frågar om ruta från användaren
num3 :: IO ()
num3 = do
  putStrLn "Choose a square that you want to assign a value to"
  sqr <- getLine
  if notElem sqr squares 
    then putStrLn "Not a valid square"
    else askForNumber sqr

-- frågar om nummer från användaren
askForNumber :: String -> IO ()
askForNumber sqr = do
  putStrLn "Enter a number to put into the square"
  input <- getLine
  case readMaybe input of
    Just number
      | number >= 0 && number <= 9 -> assignValSquare sqr number
      | otherwise -> do
          putStrLn "Please enter a number between 0 and 9."
          askForNumber sqr
    Nothing -> do
      putStrLn "Please enter a valid number."
      askForNumber sqr


-- assignar numret som användaren skriver till squaren den valt
-- om numret ej är giltigt och gör sudokut olösligt så frågar vi om användaren verkligen vill tilldela numret
-- annars bara tilldela
assignValSquare :: String -> Int -> IO () -- ska en board skickas in också


