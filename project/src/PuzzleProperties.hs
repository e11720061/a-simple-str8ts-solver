module PuzzleProperties where

import PuzzleTypes ( black, blank, Grid, Matrix, Square )
import Data.List ( transpose )

--------------------------------------------------

blackCellCount :: Grid -> Int
blackCellCount = cellCount black

blankCellCount :: Grid -> Int
blankCellCount = cellCount blank

--------------------------------------------------

cellCount :: (Square -> Bool) -> Grid -> Int
cellCount hasProperty grid = sum (concat binaryGrid)
    where binaryGrid =  gridToBinary hasProperty grid

isSymmetrical :: Grid -> Bool
isSymmetrical grid = binaryGrid == transpose binaryGrid
    where binaryGrid = gridToBinary black grid

--------------------------------------------------

gridToBinary :: (Square -> Bool) -> Grid -> Matrix Int
gridToBinary hasProperty = map (map (squareToBinary hasProperty))

squareToBinary :: (Square -> Bool) -> Square -> Int
squareToBinary hasProperty square = if hasProperty square then 1 else 0