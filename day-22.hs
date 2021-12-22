#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array --package PSQueue --package mtl

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Array

main :: IO ()
main = do
  steps <- fmap parseInput getContents
  print $ countOnCubes (R (-50,50) (-50,50) (-50,50)) steps

composeN :: (a -> a) -> Int -> a -> a
composeN _ 0 a = a
composeN fn n a = composeN fn (n-1) (fn a)

biSplit :: Eq a => [a] -> [a] -> ([a], [a])
biSplit delim s = (a, concat (b:c))
  where (a:b:c) = splitOn delim s

mapFirst :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapFirst _ _ [] = []
mapFirst pred act (a:as)
  | pred a = act a:as
  | otherwise = a:mapFirst pred act as

cmpFst :: Ord a => (a, b) -> (a, b) -> Ordering
cmpFst (a1, _) (a2, _) = a1 `compare` a2

cmpSnd :: Ord b => (a, b) -> (a, b) -> Ordering
cmpSnd (_, b1) (_, b2) = b1 `compare` b2

data Region = R { x :: (Int, Int), y :: (Int, Int), z :: (Int, Int) } deriving (Eq, Show)
data Step = On { region :: Region } | Off { region :: Region } deriving (Eq, Show)
type Cube = (Int, Int, Int)

parseRegion :: String -> Region
parseRegion s = R (read x1, read x2) (read y1, read y2) (read z1, read z2)
  where
    (x1:x2:y1:y2:z1:z2:[]) = wordsBy (not . \c -> c == '-' || isDigit c) s

parseStep :: String -> Step
parseStep s =
  case stepType of
    "on" -> On region
    "off" -> Off region
  where
    (stepType, regionStr) = biSplit " " s
    region = parseRegion regionStr

parseInput :: String -> [Step]
parseInput = map parseStep . lines

cubeInRegion :: Region -> Cube -> Bool
cubeInRegion (R xs ys zs) (x, y, z) = inRange xs x && inRange ys y && inRange zs z
  where inRange (a, b) c = a <= c && c <= b

isCubeOn :: [Step] -> Cube -> Bool
isCubeOn steps c = foldl applyStep False steps
  where
    applyStep isOn (On r) = if cubeInRegion r c then True else isOn
    applyStep isOn (Off r) = if cubeInRegion r c then False else isOn

countOnCubes :: Region -> [Step] -> Int
countOnCubes (R (x1,x2) (y1,y2) (z1,z2)) steps = length [1 | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2], isCubeOn steps (x, y, z)]
