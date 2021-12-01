#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script

main :: IO ()
main = do
  measurements <- getContents
  putStrLn $ show $ countIncreasedPings $ map read $ lines measurements

countIncreasedPings :: [Int] -> Int
countIncreasedPings = snd . foldl (\(prev, count) current -> (current, if current > prev then count + 1 else count)) (maxBound, 0)
