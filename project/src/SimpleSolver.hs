module SimpleSolver where

import PuzzleTypes (black, blank, columns, empty, rows, size, values, whiteCompartments, Color(White), Matrix, RowOrColumn, Solver, Square(..))

--------------------------------------------------

solve :: Solver
solve = filter valid . completions

completions :: Matrix Square -> Matrix Choices
completions = expand . choices

--------------------------------------------------

type Choices = [Square]

candidates :: Choices
candidates = [Square { color = White, value = v } | v <- [1..size]]

choices :: Matrix Square -> Matrix Choices
choices = map (map choice)

choice :: Square -> Choices
choice s = if blank s then candidates else [s]

--------------------------------------------------

expand :: [[[a]]] -> [[[a]]]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

--------------------------------------------------

valid :: Matrix Square -> Bool
valid g = all validRowOrColumn (rows g) && all validRowOrColumn (columns g)

validRowOrColumn :: RowOrColumn Square -> Bool
validRowOrColumn ss = condition1 ss && condition2 ss

--------------------------------------------------

condition1 :: RowOrColumn Square -> Bool
condition1 ss = noDuplicates [value s | s <- ss, not (black s && empty s)]

noDuplicates :: Eq a => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = notElem x xs && noDuplicates xs

--------------------------------------------------

-- condition2 :: [Square] -> Bool
-- condition2 = all (straight . values) . whiteCompartments

-- straight :: [Int] -> Bool
-- straight = noGaps . sort

-- noGaps :: [Int] -> Bool
-- noGaps [] = True
-- noGaps [x] = True
-- noGaps (x:xs) = x + 1 == head xs && noGaps xs

--------------------------------------------------

condition2 :: RowOrColumn Square -> Bool
condition2 = all (minMaxCheck . values) . whiteCompartments

minMaxCheck :: [Int] -> Bool
minMaxCheck xs = maximum xs - minimum xs < length xs
