module Main (main) where

import RuntimeAnalyser
import DateRange
import PuzzleDownloader
import AdvancedSolver

--------------------------------------------------

puzzleFolderPath :: PuzzleFolderPath
puzzleFolderPath = "../puzzles"

-- must point to an existing .csv file 
outputCsvPath :: OutputCsvPath
outputCsvPath = "../analysis/analysis.csv"

--------------------------------------------------

startDate :: StartDate
startDate = "01/01/2023"

endDate :: EndDate
endDate = "02/01/2023"

downloadPuzzles :: IO ()
downloadPuzzles = savePuzzlesAsTxt puzzleFolderPath startDate endDate

--------------------------------------------------

analyzePuzzles :: IO ()
analyzePuzzles = readAndAnalyzePuzzles outputCsvPath puzzleFolderPath solve

--------------------------------------------------

main :: IO ()
main = do
    putStrLn "A Simple Str8ts Solver"
    downloadPuzzles
    analyzePuzzles