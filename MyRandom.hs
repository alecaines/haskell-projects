{-
File: MyRandom.hs

Alexander Caines

Purpose: For random integers using a monad.
Addapted from Thompson's book on Haskell, third edition, page 464.
-}

module MyRandom where

import Data.Time.Clock
import Data.Time.Format

--Returns True or false depending on whether a randomly generated number
--is greater than the difference between 100 and the a certain probability
--range
probability :: Float -> IO Bool
probability probFactor = do
    let probRange = round (100 * probFactor)
    randomValue <- randomInt 100
    return (randomValue > (100 - probRange))


--Function that returns True once it has counted down from the given number
delay :: Int -> Bool
delay 0 = (1>0)
delay n = delay (n-1)

-- Monadic function returns a random number between 0 and n - 1, inclusive.
randomInt :: Int -> IO Int
randomInt n = do
    time <- getCurrentTime

    if (delay 10000) then do
        return ((`rem` n) $ read $ take 6 $
                          formatTime defaultTimeLocale "%q" time)
    else
        return ((`rem` n) $ read $ take 6 $
                          formatTime defaultTimeLocale "%q" time)


-- Returns a random item from a list.
pickRandom :: [a] -> IO a
pickRandom list = do
    index <- randomInt (length list)
    return (list !! index)

-- Prints random integers between 0 and n - 1, for
-- a given number of iterations.
testRandomInt :: Int -> Int -> IO ()
testRandomInt n iterations = do
    if iterations == 0 then
        return ()
    else do
        r <- randomInt n
        putStrLn (show r)
        testRandomInt n (iterations -1)
