#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array

import Data.List
import Data.Ord
import Data.Array.IArray
import qualified Data.Set as S

main :: IO ()
main = do
  floorDepths <- fmap parseInput getContents
  putStrLn $ show floorDepths
  putStrLn $ show $ calculateRisk floorDepths
  putStrLn $ show $ largestBasins floorDepths 3

type DepthMap = Array (Int, Int) Int

parseInput :: String -> DepthMap
parseInput s = array ((1,1), (colCount, rowCount)) $ concat [[ ((x, y), read $ i:[] :: Int) | (i, x) <- zip row [1..] ] | (row, y) <- zip rows [1..]]
  where
    rows@(first:_) = lines s
    rowCount = length rows
    colCount = length first

isLowPoint :: DepthMap -> (Int, Int) -> Bool
isLowPoint m p = all id $ map (isLower m p) [north p, east p, south p, west p]

riskLevel :: Int -> Int
riskLevel = (+1)

isLower :: DepthMap -> (Int, Int) -> (Int, Int) -> Bool
isLower m p p'
  | inRange (bounds m) (p') = m ! p < m ! p'
  | otherwise = True

calculateRisk :: DepthMap -> Int
calculateRisk m = sum $ map (riskLevel . (m!)) $ filter (isLowPoint m) $ indices m

basin :: DepthMap -> (Int, Int) -> S.Set (Int, Int)
basin m p
  | inRange (bounds m) p && m ! p < 9 = S.unions [ S.singleton p, northB, eastB, southB, westB ]
  | otherwise = S.empty
  where
    subBasin dir = let p' = dir p in if isLower m p p' then basin m p' else S.empty
    northB = subBasin north
    eastB = subBasin east
    southB = subBasin south
    westB = subBasin west

north :: (Int, Int) -> (Int, Int)
north (x, y) = (x, y-1)

south :: (Int, Int) -> (Int, Int)
south (x, y) = (x, y+1)

east :: (Int, Int) -> (Int, Int)
east (x, y) = (x+1, y)

west :: (Int, Int) -> (Int, Int)
west (x, y) = (x-1, y)

allBasins :: DepthMap -> [S.Set (Int, Int)]
allBasins m = map (basin m) $ filter (isLowPoint m) $ indices m

largestBasins :: DepthMap -> Int -> Int
largestBasins m n = product $ take n $ sortOn Down $ map S.size $ allBasins m
