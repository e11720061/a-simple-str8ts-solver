module PuzzleDownloader where

--------------------------------------------------

import FileWriter ( writeInto, FolderPath )
import PuzzleScraper ( getPuzzleDataFrom )
import DateRange
    ( getDates, toJisFormat, Date, EndDate, StartDate ) 

--------------------------------------------------

savePuzzlesAsTxt :: FolderPath -> StartDate -> EndDate -> IO ()
savePuzzlesAsTxt = forEachDateDo savePuzzleAsTxt

--------------------------------------------------

type Action = FolderPath -> Date -> IO ()

forEachDateDo :: Action -> FolderPath -> StartDate -> EndDate -> IO()
forEachDateDo action folderPath startDate endDate =
    mapM_ (\date -> do action folderPath date) (getDates startDate endDate)

--------------------------------------------------

savePuzzleAsTxt :: Action
savePuzzleAsTxt folderPath date = do
    maybePuzzleData <- getPuzzleDataFrom date
    case maybePuzzleData of
        Nothing -> putStrLn ("savePuzzleAsTxt: failed for date " ++ date)
        Just puzzleData -> writeInto folderPath fileName content
            where content = show puzzleData
            where fileName = toJisFormat date ++ ".txt"