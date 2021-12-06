#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package array

import Data.List.Split
import Data.Array((!), listArray)

main :: IO ()
main = do
  initialFish <- fmap parseFish $ getContents
  putStrLn $ show $ initialFish
  putStrLn $ show $ simulateDay initialFish
  putStrLn $ show $ simulate 1 initialFish
  putStrLn $ show $ simulate 18 initialFish
  putStrLn $ show $ simulate 80 initialFish
  putStrLn $ show $ simulate 256 initialFish

parseFish :: String -> [Int]
parseFish = map read . splitOn ","

simulateDay :: [Int] -> [Int]
simulateDay (0:fs) = 6:8:simulateDay fs
simulateDay (x:fs) = (x-1):simulateDay fs
simulateDay [] = []

simulate :: Int -> [Int] -> Int
simulate d fs = sum $ map (simulateSingle d) fs

simulateSingle :: Int -> Int -> Int -- days -> fish -> fishCount
simulateSingle d f = 1 + spawnCount (d - f)

spawnCount :: Int -> Int -- days -> fishCount (for single fish at 0)
spawnCount n
  | n >= 0 = r!n
  | otherwise = 0
  where
    r = listArray (0, n) (map f [0..n])
    f 0 = 0
    f i = 1 + lookup (i - 7) + lookup (i - 9)
    lookup i = if i < 0 then 0 else r!i
