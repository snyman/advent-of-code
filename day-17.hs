#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array --package PSQueue --package mtl

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Control.Monad.State

main :: IO ()
main = do
  target <- fmap parseTarget getContents
  putStrLn $ show $ target
  putStrLn $ show $ take 20 $ xPositions 0 7
  putStrLn $ show $ take 20 $ yPositions 0 2
  putStrLn $ show $ possibleXs target
  putStrLn $ show $ possibleYs target
  let launches = possibleLaunches target
  putStrLn $ show $ launches
  let best = bestLaunch launches
  putStrLn $ show $ best
  putStrLn $ show $ maxHeight $ snd best

biSplit :: Eq a => [a] -> [a] -> ([a], [a])
biSplit delim s = (a, concat (b:c))
  where (a:b:c) = splitOn delim s

data Area = Area { xs :: (Int, Int), ys :: (Int, Int) } deriving(Show, Eq)

parseTarget :: String -> Area
parseTarget s = Area (read x1, read x2) (read y1, read y2)
  where
    (x1:x2:y1:y2:[]) = wordsBy (not . \c -> c == '-' || isDigit c) s

xPositions :: Int -> Int -> [Int]
xPositions p 0 = []
xPositions p v = newPos:(xPositions newPos newVel)
  where
    newPos = p + v
    newVel = if v < 0 then v + 1 else v - 1

extend :: [a] -> [a]
extend [] = []
extend [a] = repeat a
extend (a:as) = a:(extend as)

yPositions :: Int -> Int -> [Int]
yPositions p v = newPos:(yPositions newPos newVel)
  where
    newPos = p + v
    newVel = v - 1

inRange :: (Int, Int) -> Int -> Bool
inRange (min, max) n = n >= min && n <= max

xHits :: Area -> Int -> Bool
xHits (Area xs _) v = any (inRange xs) $ xPositions 0 v

yHits :: Area -> Int -> Bool
yHits (Area _ ys@(minY, _)) v = any (inRange ys) $ takeWhile (>= minY) $ yPositions 0 v

possibleXs :: Area -> [Int]
possibleXs a@(Area (_, maxX) _) = filter (xHits a) [0..maxX]

possibleYs :: Area -> [Int]
possibleYs a@(Area _ (minY, maxY)) = filter (yHits a) [minY..(-minY)]

launchHits :: Area -> (Int, Int) -> Bool
launchHits (Area xs ys@(minY,_)) (vx, vy) = any (\(x, y) -> inRange xs x && inRange ys y) coords
  where
    coords = zip (extend $ xPositions 0 vx) (takeWhile (>= minY) $ yPositions 0 vy)

possibleLaunches :: Area -> [(Int, Int)]
possibleLaunches a = do
  vx <- possibleXs a
  vy <- possibleYs a
  if launchHits a (vx, vy) then return (vx, vy) else []

maxHeight :: Int -> Int
maxHeight vy = fst $ head $ filter (\(y1, y2) -> y1 > y2) $ zip yPos (drop 1 yPos)
  where yPos = yPositions 0 vy

bestLaunch :: [(Int, Int)] -> (Int, Int)
bestLaunch = maximumBy (\(_, y1) (_, y2) -> maxHeight y1 `compare` maxHeight y2)
