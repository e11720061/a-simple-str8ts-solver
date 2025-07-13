module AdvancedSolver where

import PuzzleTypes
    ( columns,
      rows,
      size,
      white,
      Color(White),
      Compartment,
      Grid,
      Matrix,
      Row,
      Solver,
      Square(..) )
import SimpleSolver ( choices, condition1, condition2, Choices )
import Data.List ( groupBy )

--------------------------------------------------

-- solve :: Solver
-- solve = filter valid . expand . prune . choices

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy rows . pruneBy columns

pruneBy :: (Matrix Choices -> Matrix Choices) -> Matrix Choices -> Matrix Choices
pruneBy f = f . map pruneRow . f

--------------------------------------------------

pruneRow :: Row Choices -> Row Choices
pruneRow = pruneRowByCondition1 . pruneRowByCondition2

--------------------------------------------------

pruneRowByCondition1 :: Row Choices -> Row Choices
pruneRowByCondition1 row = map (remove fixed) row
    where fixed = [s | [s] <- row]

remove :: [Square] -> [Square] -> [Square]
remove _ [] = error "not possible"
remove [] ys = ys
remove _ [y] = [y]
remove (x:xs) ys = remove xs [ y | y <- ys, value y /= value x ]

--------------------------------------------------

pruneRowByCondition2 :: Row Choices -> Row Choices
pruneRowByCondition2 = concatMap pruneCompartmentByCondition2 . AdvancedSolver.compartments

compartments :: Row Choices -> [Compartment Choices]
compartments = groupBy (\x y -> color (head x) == color (head y))

isWhiteCompartment :: Compartment Choices -> Bool
isWhiteCompartment = white . head . head

pruneCompartmentByCondition2 :: Row Choices -> Row Choices
pruneCompartmentByCondition2 compartment
    | AdvancedSolver.isWhiteCompartment compartment = pruneWhiteCompartmentByCondition2 compartment
    | otherwise = compartment

pruneWhiteCompartmentByCondition2 :: Row Choices -> Row Choices
pruneWhiteCompartmentByCondition2 compartment = case [value s | [s] <- compartment] of
    [] -> compartment
    fixed -> map (remove impossible) compartment
        where range = length compartment - 1
              maxValue = minimum fixed + range
              minValue = maximum fixed - range
              impossible = [Square { color = White, value = v } | v <- [1..size], v > maxValue || v < minValue]

--------------------------------------------------

solve :: Solver
solve = search . choices

search :: Matrix Choices -> [Grid]
search m
    | not (safe m) = []
    | complete m' = [map (map head) m']
    | otherwise = concatMap search (expand1 m')
    where m' = prune m

expand1 :: Matrix [a] -> [Matrix [a]]
expand1 m = [rows1 ++ [row1 ++ [c]: row2] ++ rows2 | c <- cs]
    where
        (rows1, row:rows2) = break (any smallest) m
        (row1, cs:row2) = break smallest row
        smallest x = length x == n
        n = minimum (counts m)
        counts = filter (> 1) . map length . concat

--------------------------------------------------

safe :: Matrix Choices -> Bool
safe m = all ok (rows m) && all ok (columns m)

ok :: Row Choices -> Bool
ok xs = okByCondition1 xs && okByCondition2 xs

okByCondition1 :: Row Choices -> Bool
okByCondition1 row = condition1 [s | [s] <- row]

okByCondition2 :: Row Choices -> Bool
okByCondition2 row = condition2  [s | c <- completeCompartments row, [s] <- c]

completeCompartments :: Row Choices -> [Compartment Choices]
completeCompartments = filter (all singleton) . AdvancedSolver.compartments

complete :: Matrix [a] -> Bool
complete = all (all singleton)

singleton :: [a] -> Bool
singleton [_] = True
singleton _ = False