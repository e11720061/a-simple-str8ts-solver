{-# LANGUAGE OverloadedStrings #-}
module PuzzleScraper where

import Text.HTML.Scalpel
    ( scrapeURL,
      attr,
      chroots,
      text,
      (@:),
      hasClass,
      anySelector,
      Scraper )
import PuzzleTypes ( size, Color(..), Grid, Square(Square) )
import Data.Char ( isPunctuation )
import Data.List ( isInfixOf )

--------------------------------------------------

type PuzzleData = (Date, Difficulty, Grid, Solution)

type Date = String
type Difficulty = String
type Solution = Grid

--------------------------------------------------

getPuzzleDataFrom :: Date -> IO (Maybe PuzzleData)
getPuzzleDataFrom date = scrapeURL (getURL date) (getData date)

--------------------------------------------------

getURL :: [Char] -> [Char]
getURL date = concat [
    "https://www.str8ts.com/",
    "Print_Daily_Str8ts.aspx?",
    "solution=please&lang=en&day=",
    date]

--------------------------------------------------

getData :: Date -> Scraper String PuzzleData
getData date = do
    metaInfo <- getMetaInfo
    cellInfos <- getCellInfos
    let difficulty = metaInfoToDifficulty metaInfo
    let grid = cellInfosToGrid cellInfos
    let solvedGrid = cellInfosToSolvedGrid cellInfos
    return (date, difficulty, grid, solvedGrid)

--------------------------------------------------

getMetaInfo :: Scraper String [String]
getMetaInfo = chroots "font" (text anySelector)

--------------------------------------------------

metaInfoToDifficulty :: [String] -> Difficulty
metaInfoToDifficulty [_,difficulty] = filter (not . isPunctuation) (words difficulty !! 1)
metaInfoToDifficulty _ = error "Error in metaInfoToDifficulty: Wrong format"

--------------------------------------------------

getCellInfos :: Scraper String [(String, String)]
getCellInfos = chroots ("td" @: [hasClass "InnerDone"]) getCellInfo

getCellInfo :: Scraper String (String, String)
getCellInfo = do
    valueString <- text anySelector
    className <- attr "class" anySelector
    return (className, valueString)

--------------------------------------------------

cellInfosToGrid :: [(String, String)] -> Grid
cellInfosToGrid cellInfos
    | length cellInfos /= size * size = error "Error in cellInfosToGrid: size of grid is wrong"
    | otherwise = groupInto9 (map cellInfoToSquare cellInfos)

cellInfoToSquare :: (String, String) -> Square
cellInfoToSquare (className, valueString)
    | "bluecell" `isInfixOf` className = Square White 0
    | otherwise = Square (mapClassNameToColor className) (mapStringToValue valueString)

--------------------------------------------------

cellInfosToSolvedGrid :: [(String, String)] -> Solution
cellInfosToSolvedGrid = groupInto9 . map cellInfoToSolvedSquare

cellInfoToSolvedSquare :: (String, String) -> Square
cellInfoToSolvedSquare (className, valueString) = Square (mapClassNameToColor className) (mapStringToValue valueString)

--------------------------------------------------

mapClassNameToColor :: String -> Color
mapClassNameToColor x
    | "bcell" `isInfixOf` x = Black
    | otherwise = White

mapStringToValue :: String -> Int
mapStringToValue "1" = 1
mapStringToValue "2" = 2
mapStringToValue "3" = 3
mapStringToValue "4" = 4
mapStringToValue "5" = 5
mapStringToValue "6" = 6
mapStringToValue "7" = 7
mapStringToValue "8" = 8
mapStringToValue "9" = 9
mapStringToValue _ = 0

--------------------------------------------------


groupInto9 :: [a] -> [[a]]
groupInto9  = groupIntoN 9

groupIntoN :: Int -> [a] -> [[a]]
groupIntoN _ [] = []
groupIntoN n xs = take n xs : groupInto9 (drop n xs)
