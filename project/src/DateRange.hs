module DateRange where

--------------------------------------------------

import Data.Time
    ( addDays,
      diffDays,
      formatTime,
      defaultTimeLocale,
      parseTimeOrError,
      Day )

--------------------------------------------------

type Format = String

usaFormat :: Format
usaFormat = "%d/%m/%Y"

jisFormat :: Format
jisFormat = "%Y-%m-%d"

--------------------------------------------------

type StartDate = Date
type EndDate = Date
type Date = String

getDates :: StartDate -> EndDate -> [Date]
getDates from to = map dateToString dates
    where
        fromDate = stringToDate from
        toDate = stringToDate to
        dates = [addDays n fromDate | n <- [0..diffDays toDate fromDate]]

--------------------------------------------------

stringToDate :: String -> Day
stringToDate = stringToDateWithFormat usaFormat

dateToString :: Day -> String
dateToString = dateToStringWithFormat usaFormat

--------------------------------------------------

toJisFormat :: Date -> Date
toJisFormat = reformat usaFormat jisFormat

reformat :: Format -> Format -> Date -> Date
reformat oldFormat newFormat date = dateToStringWithFormat newFormat (stringToDateWithFormat oldFormat date)

--------------------------------------------------

stringToDateWithFormat :: Format -> String -> Day
stringToDateWithFormat = parseTimeOrError False defaultTimeLocale

dateToStringWithFormat :: Format -> Day -> String
dateToStringWithFormat = formatTime defaultTimeLocale