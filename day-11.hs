#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array

import Data.List
import Control.Monad
import Data.Either
import Data.Array.IArray
import Data.Foldable (toList)

main :: IO ()
main = do
  octos <- fmap parseInput getContents
  putStrLn $ show $ octos
  putStrLn $ show $ runSteps octos 100

type Octo = Either Int Int
type OctoMap = Array (Int, Int) Octo

parseInput :: String -> OctoMap
parseInput s = array ((1,1), (colCount, rowCount)) $ concat [[ ((x, y), Right $ read $ i:[]) | (i, x) <- zip row [1..] ] | (row, y) <- zip rows [1..]]
  where
    rows@(first:_) = lines s
    rowCount = length rows
    colCount = length first

validPoint :: OctoMap -> (Int, Int) -> Bool
validPoint m = inRange $ bounds m

adjacencies :: OctoMap -> (Int, Int) -> [(Int, Int)]
adjacencies m (x, y) = filter (validPoint m) $ do
  changeX <- [-1, 0, 1]
  changeY <- [-1, 0, 1]
  if (changeX, changeY) /= (0, 0)
    then [(x + changeX, y + changeY)]
    else []

increaseEnergy :: OctoMap -> (Int, Int) -> OctoMap
increaseEnergy m p =
  let oldOcto = m ! p
      newOcto = incrementOcto oldOcto
      newMap = m // [(p, newOcto)]
  in
  case (oldOcto, newOcto) of
    (Right _, Left _) -> foldl increaseEnergy newMap $ adjacencies newMap p
    _ -> newMap

incrementOcto :: Octo -> Octo
incrementOcto (Right 9) = Left 10
incrementOcto (Left x) = Left (x + 1)
incrementOcto (Right x) = Right (x + 1)

isFlashed :: Octo -> Bool
isFlashed (Left _) = True
isFlashed _ = False

countFlashes :: OctoMap -> Int
countFlashes = length . (filter isFlashed) . toList

resetFlashes :: OctoMap -> OctoMap
resetFlashes = amap unFlash
  where unFlash (Left _) = Right 0
        unFlash x = x

runStep :: (OctoMap, Int) -> (OctoMap, Int)
runStep (m, f) = let
  incremented = foldl increaseEnergy m $ indices m
  flashed = countFlashes incremented
  reset = resetFlashes incremented
  in (reset, f + flashed)

runSteps :: OctoMap -> Int -> (OctoMap, Int)
runSteps m n = foldl (\mf _ -> runStep mf) (m, 0) [1..n]
