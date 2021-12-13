#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array

import Data.List.Split
import qualified Data.Set as S

main :: IO ()
main = do
  (dots, folds) <- fmap parseInput getContents
  putStrLn $ show dots
  putStrLn $ show folds
  let firstFold = foldDots dots (folds !! 0)
  putStrLn $ show $ firstFold
  putStrLn $ show $ S.size firstFold

biSplit :: Eq a => [a] -> [a] -> ([a], [a])
biSplit delim s = (a, concat (b:c))
  where (a:b:c) = splitOn delim s

data Fold = Vert { x :: Int } | Horiz { y :: Int } deriving (Eq, Show)
type Dots = S.Set (Int, Int)

parseDot :: String -> (Int, Int)
parseDot s = (read x, read y)
  where (x, y) = biSplit "," s

parseDots :: [String] -> Dots
parseDots = foldl (flip (S.insert . parseDot)) S.empty

parseFold :: String -> Fold
parseFold s =
  case axis of
    "y" -> Horiz $ read coord
    "x" -> Vert $ read coord
  where
    line = words s !! 2
    (axis, coord) = biSplit "=" line

parseFolds :: [String] -> [Fold]
parseFolds = foldr (\s fs -> (parseFold s) : fs) []

parseInput :: String -> (Dots, [Fold])
parseInput ss = (parseDots dotLines, parseFolds foldLines)
  where (dotLines, foldLines) = biSplit [""] $ lines ss

foldX :: Int -> (Int, Int) -> (Int, Int)
foldX x' (x, y) =
  if x > x' then (2*x' - x, y)
  else (x, y)

foldY :: Int -> (Int, Int) -> (Int, Int)
foldY y' (x, y) =
  if y > y' then (x, 2*y' - y)
  else (x, y)

foldDots :: Dots -> Fold -> Dots
foldDots ds (Vert x) = S.map (foldX x) ds
foldDots ds (Horiz y) = S.map (foldY y) ds
