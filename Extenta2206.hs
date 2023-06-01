import Data.List

-- 1
permutate :: (Eq a) => [a] -> [[a]]
permutate [] = [[]]
permutate list = [x : xs | x <- list, xs <- permutate $ delete x list]

-- 2a
f :: Int -> [Char]
f n = replicate n '+'

g :: [Int] -> [[Char]] -- Answer
g xs = [f x | x <- xs, x > 3]

-- 2b
-- curries :: a1 -> b1 -> a2 -> b2 -> c
-- curries = curry curry -- Error

-- 2c
curries2 :: (((a, b1), b2) -> c) -> a -> b1 -> b2 -> c
curries2 = curry . curry

-- 3
g1 :: [[Int]] -> [[Int]]
g1 list = [tail x | x <- list, not (null x), (odd . head) x]

g2 :: [[Int]] -> [[Int]]
g2 list = map tail $ filter (odd . head) $ filter (not . null) list

-- 4
-- "Ukraine" >>= (\u -> flip (:) [] $ id u)
-- (\u -> flip (:) [] u)
-- (\u -> (:) u [])
-- (\u -> [u])
-- :: [Char]

-- 5
data Relation a b = Relation [(a, b)]

union :: Relation a b -> Relation a b -> Relation a b
union (Relation r1) (Relation r2) = Relation $ union r1 r2

intersection :: Relation a b -> Relation a b -> Relation a b
intersection (Relation r1) (Relation r2) = Relation $ intersect r1 r2

composition :: Relation b c -> Relation a b -> Relation a c
composition (Relation bc) (Relation ab) = Relation [(x, z) | (x, y1) <- r2, (y2, z) <- r1, y1 == y2]


equalR :: (Eq a, Eq b) => Relation a b ->    Relation a b -> Bool
equalR (Relation r1) (Relation r2) = subrel (Relation r1) (Relation r2) && subrel (Relation r2) (Relation r1)
-- just to make sure that order of elements does not matter

subrel :: (Eq a, Eq b) => Relation a b -> Relation a b -> Bool
subrel (Relation r1) (Relation r2) = all ((flip elem) r2) r1
-- or simply subset

closure :: Relation a a -> Relation a a
closure (Relation r)
  | equalR (Relation r) (union (Relation r) (composition (Relation r) (Relation r))) = Relation r
  | otherwise = closure (union (Relation r) (composition (Relation r) (Relation r)))
