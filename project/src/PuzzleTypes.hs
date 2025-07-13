module PuzzleTypes where

import Data.List ( groupBy, transpose )

--------------------------------------------------

type Matrix a = [Row a]
type RowOrColumn a = [a]
type Row a = [a]
type Compartment a = [a]

size :: Int
size = 9

type Grid = Matrix Square

data Square = Square { color :: Color, value :: Value } deriving (Show, Eq, Read)
data Color = White | Black deriving (Show, Eq, Read)
type Value = Int

blank :: Square -> Bool
blank s = empty s && white s

empty :: Square -> Bool
empty = (== 0) . value

white :: Square -> Bool
white = not . black

black :: Square -> Bool
black = (== Black) . color

--------------------------------------------------

rows :: Matrix a -> Matrix a
rows = id

columns :: Matrix a -> Matrix a
columns = transpose

compartments :: [Square] -> [[Square]]
compartments = groupBy (\x y -> color x == color y)

whiteCompartments :: [Square] -> [[Square]]
whiteCompartments = filter isWhiteCompartment . compartments

isWhiteCompartment :: [Square] -> Bool
isWhiteCompartment = white . head

values :: [Square] -> [Value]
values = map value

--------------------------------------------------

type Solver = Grid -> [Grid]