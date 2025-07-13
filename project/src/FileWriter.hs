module FileWriter where

import System.Directory ( createDirectoryIfMissing, listDirectory )
import System.IO ( hPutStrLn, IOMode(AppendMode), withFile )

--------------------------------------------------

type FolderPath = String
type FileName = String
type Content = String

--------------------------------------------------

-- overwrites what was in the file before
writeInto :: FolderPath -> FileName -> Content -> IO ()
writeInto folderPath fileName content = do
    createDirectoryIfMissing True folderPath
    let filePath = folderPath ++ "/" ++ fileName
    writeFile filePath content

writeAsNewLineInto :: FilePath -> Content -> IO ()
writeAsNewLineInto filePath text = 
    withFile filePath AppendMode $ \handle -> do
        hPutStrLn handle text

--------------------------------------------------

getFilePaths :: FolderPath -> IO [FilePath]
getFilePaths folderPath = do
    filePaths <- listDirectory folderPath
    let relativeFilePaths = map (\filePath -> folderPath ++ "/" ++ filePath) filePaths
    return relativeFilePaths