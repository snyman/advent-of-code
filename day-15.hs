#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array --package PSQueue

import Data.List
import Data.List.Split
import Data.Array.IArray
import qualified Data.Map as M
import qualified Data.PSQueue as PSQ

main :: IO ()
main = do
  risk <- fmap parseInput getContents
  putStrLn $ show risk
  let path = aStar risk
  putStrLn $ show $ path
  putStrLn $ show $ (totalRisk <$> (Just risk) <*> path)
  let expanded = expand risk 5
  let expandedPath = aStar expanded
  putStrLn $ show $ expandedPath
  putStrLn $ show $ (totalRisk <$> (Just expanded) <*> expandedPath)

biSplit :: Eq a => [a] -> [a] -> ([a], [a])
biSplit delim s = (a, concat (b:c))
  where (a:b:c) = splitOn delim s

type Coord = (Int, Int)
type RiskMap = Array Coord Int

parseInput :: String -> RiskMap
parseInput s = array ((1,1), (colCount, rowCount)) $ concat [[ ((x, y), read $ i:[]) | (i, x) <- zip row [1..] ] | (row, y) <- zip rows [1..]]
  where
    rows@(first:_) = lines s
    rowCount = length rows
    colCount = length first

expand :: RiskMap -> Int -> RiskMap
expand risk n = array ((1,1), (colCount, rowCount)) $ concat [[((x, y), valAt (x, y)) | x <- [1..colCount]] | y <- [1..rowCount]]
  where
    (_, (cols, rows)) = bounds risk
    rowCount = rows * n
    colCount = cols * n
    valAt (x, y) =
      let tileX = floor (fromIntegral (x-1) / fromIntegral cols)
          tileY = floor (fromIntegral (y-1) / fromIntegral rows)
          origX = x - (tileX * cols)
          origY = y - (tileY * rows)
          unboundVal = (risk ! (origX, origY)) - 1 + tileX + tileY
      in (unboundVal `mod` 9) + 1

type Score = M.Map Coord Int

gScore :: Score -> Coord -> Int
gScore s c = M.findWithDefault maxBound c s

hScore :: RiskMap -> Coord -> Int
hScore r (x, y) = (maxX - x) + (maxY - y)
  where (maxX, maxY) = snd $ bounds r

fScore :: Score -> RiskMap -> Coord -> Int
fScore s r c =
  case M.lookup c s of
   (Just gScore) -> gScore + hScore r c
   Nothing -> maxBound

data AStar = AStar { risk :: RiskMap, score :: Score, from :: M.Map Coord Coord, open :: PSQ.PSQ Coord Int }

aStar :: RiskMap -> Maybe [Coord]
aStar r = aStar' $ AStar r gScoreMap M.empty openSet
  where
    (start, end) = bounds r
    gScoreMap = M.singleton start 0
    openSet = PSQ.singleton start $ fScore gScoreMap r start

aStar' :: AStar -> Maybe [Coord]
aStar' (AStar risk score from open) = do
  (current, open') <- PSQ.minView open
  let currentCoord = PSQ.key current
  let goal = snd $ bounds risk
  if currentCoord == goal
    then return $ reconstructPath from currentCoord
    else do
      let new = foldl (processNeighbour currentCoord) (AStar risk score from open') $ adjacencies risk currentCoord
      aStar' new

reconstructPath :: M.Map Coord Coord -> Coord -> [Coord]
reconstructPath from coord = case (M.lookup coord from) of
  (Just prev) -> coord:(reconstructPath from prev)
  Nothing -> [coord]

processNeighbour :: Coord -> AStar -> Coord -> AStar
processNeighbour current orig@(AStar risk score from open) neighbour =
  if tentativeScore < (gScore score neighbour) then
    let from' = M.insert neighbour current from
        score' = M.insert neighbour tentativeScore score
        open' = PSQ.insert neighbour (fScore score' risk neighbour) open
    in AStar risk score' from' open'
  else orig
  where
    tentativeScore = (gScore score current) + (risk ! neighbour)

validPoint :: RiskMap -> Coord -> Bool
validPoint m = inRange $ bounds m

adjacencies :: RiskMap -> Coord -> [Coord]
adjacencies m (x, y) = filter (validPoint m) $ map (\(x', y') -> (x + x', y + y')) [(1, 0), (-1, 0), (0, 1), (0, -1)]

totalRisk :: RiskMap -> [Coord] -> Int
totalRisk risk path = foldl (\score coord -> score + (risk ! coord)) (-(risk ! (1, 1)))  path
