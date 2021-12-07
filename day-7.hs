#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split

import Data.List.Split

main :: IO ()
main = do
  crabs <- fmap parseCrabs getContents
  putStrLn $ show $ findMinFuel crabs

parseCrabs :: String -> [Int]
parseCrabs = map read . splitOn ","

computeFuel :: Int -> [Int] -> Int
computeFuel d = foldl (\td c -> td + abs (c - d)) 0

findMinFuel :: [Int] -> (Int, Int)
findMinFuel cs = foldl1 minMove possibleMoves
  where
    minMove m1@(_, f1) m2@(_, f2) = if f1 < f2 then m1 else m2
    possibleMoves = [(d, computeFuel d cs) | d <- [0..maxCrab]]
    maxCrab = foldl max 0 cs

