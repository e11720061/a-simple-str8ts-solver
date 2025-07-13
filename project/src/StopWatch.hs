module StopWatch  where

import Control.Exception (evaluate)
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)

--------------------------------------------------

{- Note

For testing stopTime, you can use:

import Control.Concurrent (threadDelay)
threadDelay 5 * 10^9

As well as:

ghci> :set +s

-}

--------------------------------------------------

type TimeInNs = Integer

stopTime :: (a -> b) -> a -> IO (TimeInNs, b)
stopTime f input = do
    startTime <- getTime Monotonic
    output <- evaluate (f input)
    endTime <- getTime Monotonic
    let diff = diffTimeSpec startTime endTime
    return (toNanoSecs diff, output)
