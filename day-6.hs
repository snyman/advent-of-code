#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split

import Data.List.Split

main :: IO ()
main = do
  initialFish <- fmap parseFish $ getContents
  putStrLn $ show $ initialFish
  putStrLn $ show $ simulateDay initialFish
  putStrLn $ show $ simulate 18 initialFish
  putStrLn $ show $ length $ simulate 80 initialFish

parseFish :: String -> [Int]
parseFish = map read . splitOn ","

simulateDay :: [Int] -> [Int]
simulateDay (0:fs) = 6:8:simulateDay fs
simulateDay (x:fs) = (x-1):simulateDay fs
simulateDay [] = []

simulate :: Int -> [Int] -> [Int]
simulate 0 fs = fs
simulate x fs = simulate (x-1) $ simulateDay fs
