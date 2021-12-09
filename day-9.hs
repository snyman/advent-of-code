#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array

import Data.List
import Data.Array.IArray

main :: IO ()
main = do
  floorDepths <- fmap parseInput getContents
  putStrLn $ show floorDepths
  putStrLn $ show $ calculateRisk floorDepths

type DepthMap = Array (Int, Int) Int

parseInput :: String -> DepthMap
parseInput s = array ((1,1), (colCount, rowCount)) $ concat [[ ((x, y), read $ i:[] :: Int) | (i, x) <- zip row [1..] ] | (row, y) <- zip rows [1..]]
  where
    rows@(first:_) = lines s
    rowCount = length rows
    colCount = length first

isLowPoint :: DepthMap -> (Int, Int) -> Bool
isLowPoint m (x, y) = all id $ map (isLower m (x, y)) [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

riskLevel :: Int -> Int
riskLevel = (+1)

isLower :: DepthMap -> (Int, Int) -> (Int, Int) -> Bool
isLower m p p'
  | inRange (bounds m) (p') = m ! p < m ! p'
  | otherwise = True

calculateRisk :: DepthMap -> Int
calculateRisk m = sum $ map (riskLevel . (m!)) $ filter (isLowPoint m) $ indices m
