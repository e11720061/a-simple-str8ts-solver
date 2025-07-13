module PuzzleFormatter where

import PuzzleTypes
    ( Color(Black, White), Grid, Row, Square(Square) )

--------------------------------------------------

format :: Grid -> String
format [] = error "must not be empty"
format [v] = formatRow v
format (v:vs) = formatRow v ++ "\n" ++ format vs

formatRow :: Row Square -> String
formatRow [] = error "must not be empty"
formatRow [x] = formatSquare x
formatRow (x:xs) = formatSquare x ++ " " ++ formatRow xs

formatSquare :: Square -> String
formatSquare (Square White 0) = "."
formatSquare (Square White v) = show v
formatSquare (Square Black 0) = red "X"
formatSquare (Square Black v) = red (show v)

red :: String -> String
red s = "\ESC[91m" ++ s ++ "\ESC[0m"

--------------------------------------------------

{- Note

Styling is applied when used with printLn.
It does not work with print.

Example:

showPuzzleInConsole :: Grid -> IO()
showPuzzleInConsole grid = do
    putStrLn (format grid)

-}
