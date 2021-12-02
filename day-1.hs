#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script

main :: IO ()
main = do
  measurements <- getContents
  putStrLn $ show $ countIncreasedWindows $ map read $ lines measurements

countIncreasedWindows :: [Int] -> Int
countIncreasedWindows = snd . foldl (\(prev, count) current -> (current, if windowSum current > windowSum prev then count + 1 else count)) (maxBound, 0) . toWindows

type Window = (Int, Int, Int)

toWindows :: [Int] -> [Window]
toWindows (a:b:c:xs) = (a, b, c) : toWindows (b:c:xs)
toWindows _ = []

windowSum :: Window -> Int
windowSum (a, b, c) = a + b + c
