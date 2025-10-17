module RuntimeAnalyser where

import PuzzleScraper (PuzzleData, Solution)
import FileWriter ( getFilePaths, writeAsNewLineInto, FolderPath ) 
import StopWatch ( TimeInNs, stopTime )
import Data.List ( intercalate )
import PuzzleFormatter ( format )
import System.Timeout ( timeout )
import PuzzleTypes ( Solver )
import PuzzleProperties
    ( blackCellCount, blankCellCount, isSymmetrical )

--------------------------------------------------

timeOutInMu :: Integer
timeOutInMu = timeOutInSeconds * 10^6

timeOutInSeconds :: Integer
timeOutInSeconds = 60

excelSeparator :: String
excelSeparator = ";"

--------------------------------------------------

type TimedSolution = (TimeInNs, [Solution])

type PuzzleFolderPath = FolderPath
type PuzzlePath = FilePath
type OutputCsvPath = FilePath

--------------------------------------------------

readAndAnalyzePuzzles :: OutputCsvPath -> PuzzleFolderPath -> Solver -> IO()
readAndAnalyzePuzzles outputCsvPath puzzleFolderPath solver = do
    writeAsNewLineInto outputCsvPath (intercalate excelSeparator ["Date", "Difficulty", "Number of solutions", "Equal to official solution", "Run-time (ns)", "Number of black squares", "Number of blank squares", "Symmetrical"])
    puzzlePaths <- getFilePaths puzzleFolderPath
    mapM_ (readAndAnalyzePuzzle solver outputCsvPath) puzzlePaths

readAndAnalyzePuzzle :: Solver -> OutputCsvPath -> PuzzlePath -> IO ()
readAndAnalyzePuzzle solver outputCsvPath puzzlePath = do
    putStrLn ("\nReading file " ++ puzzlePath ++ "\n")
    puzzleData@(_, _, grid, officialSolution) <- fetchPuzzleData puzzlePath
    putStrLn "\nGrid:\n"
    putStrLn (format grid)
    putStrLn "\nOfficial solution:\n"
    putStrLn (format officialSolution)
    analyzePuzzle solver outputCsvPath puzzleData
    putStrLn "\n--------------------------------------------------\n"

--------------------------------------------------

analyzePuzzle :: Solver -> OutputCsvPath -> PuzzleData -> IO ()
analyzePuzzle solver outputCsvPath puzzleData@(_, _, grid, _) = do
    putStrLn "\nComputing solution...\n"
    result <- timeout (fromInteger timeOutInMu) (stopTime solver grid)
    case result of
        Just timedSolution -> writeToCsv outputCsvPath puzzleData timedSolution
        Nothing -> writeTimeoutToCsv outputCsvPath puzzleData

--------------------------------------------------

writeToCsv :: OutputCsvPath -> PuzzleData -> TimedSolution -> IO ()
writeToCsv outputCsvPath puzzleData timedSolution = do
    writeAsNewLineInto outputCsvPath (mapToCsv puzzleData timedSolution)

writeTimeoutToCsv :: OutputCsvPath -> PuzzleData -> IO ()
writeTimeoutToCsv outputCsvPath puzzleData = do
    writeAsNewLineInto outputCsvPath (mapTimeoutToScv puzzleData)

--------------------------------------------------

mapToCsv :: PuzzleData -> TimedSolution -> String
mapToCsv (date, difficulty, grid, officialSolution) (time, solutions) =
    intercalate excelSeparator [column1, column2, column3, column4, column5, column6, column7, column8]
        where column1 = date
              column2 = difficulty
              column3 = show (length solutions)
              column4 = show (not (null solutions) && officialSolution == head solutions)
              column5 = show time
              column6 = show (blackCellCount grid)
              column7 = show (blankCellCount grid)
              column8 = show (isSymmetrical grid)

mapTimeoutToScv :: PuzzleData -> String
mapTimeoutToScv (date, difficulty, grid, _) =
    intercalate excelSeparator [column1, column2, column3, column4, column5, column6, column7, column8]
        where column1 = date
              column2 = difficulty
              column3 = ""
              column4 = ""
              column5 = "timeout"
              column6 = show (blackCellCount grid)
              column7 = show (blankCellCount grid)
              column8 = show (isSymmetrical grid)

--------------------------------------------------

fetchPuzzleData :: FilePath -> IO PuzzleData
fetchPuzzleData filePath = do
    contents <- readFile filePath
    return (read contents)
