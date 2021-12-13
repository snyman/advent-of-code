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
  let fullFold = foldl foldDots dots folds
  printDots fullFold

biSplit :: Eq a => [a] -> [a] -> ([a], [a])
biSplit delim s = (a, concat (b:c))
  where (a:b:c) = splitOn delim s

newtype Point = Point { pt :: (Int, Int) } deriving (Eq, Show)
instance Ord Point where
  compare (Point (x1, y1)) (Point (x2, y2)) = compare (y1, x1) (y2, x2)

data Fold = Vert { x :: Int } | Horiz { y :: Int } deriving (Eq, Show)
type Dots = S.Set Point

parseDot :: String -> Point
parseDot s = Point (read x, read y)
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

foldX :: Int -> Point -> Point
foldX x' pt@(Point (x, y)) =
  if x > x' then Point (2*x' - x, y)
  else pt

foldY :: Int -> Point -> Point
foldY y' pt@(Point (x, y)) =
  if y > y' then Point (x, 2*y' - y)
  else pt

foldDots :: Dots -> Fold -> Dots
foldDots ds (Vert x) = S.map (foldX x) ds
foldDots ds (Horiz y) = S.map (foldY y) ds

showDot :: Dots -> Point -> Char
showDot ds pt = if pt `elem` ds then '#' else ' '

printDotLn :: Dots -> Int -> Int -> IO ()
printDotLn ds maxX y = putStrLn $ [showDot ds (Point (x, y)) | x <- [0..maxX]]

printDots :: Dots -> IO ()
printDots ds = mapM_ (printDotLn ds maxX) [0..maxY]
  where
    maxX = maximum $ S.map (fst . pt) ds
    maxY = maximum $ S.map (snd . pt) ds
