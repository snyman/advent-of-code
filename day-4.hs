#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package matrix --package vector

import Control.Monad
import Data.List.Split
import Data.Matrix
import Data.Vector (Vector)
import qualified Data.Vector as V

data Cell = M Int | U Int deriving Show
type Board = Matrix Cell

type Game = Either (Board, Int)

main :: IO ()
main = do
  bingo <- fmap lines getContents
  let (calls, boards) = parse bingo
  putStrLn $ show $ calls
  putStrLn $ show $ boards
  let endGame = playGame boards calls
  putStrLn $ show $ endGame
  putStrLn $ show $ score endGame

parse :: [String] -> ([Int], [Board])
parse (s:ss) = (parseCalls s, parseBoards ss)

parseCalls :: String -> [Int]
parseCalls s = map read $ splitOn "," s

parseBoards :: [String] -> [Board]
parseBoards ss = map (fromList 5 5) $ chunksOf 25 $ map U digits
  where digits = concatMap (map read . words) ss

score :: Game [Board] -> Int
score (Right _) = 0
score (Left (b, n)) = n * (foldl cellScore 0 b)
  where cellScore score (M _) = score
        cellScore score (U n) = score + n

playGame :: [Board] -> [Int] -> Game [Board]
playGame = foldM (flip callNumber)

callNumber :: Int -> [Board] -> Game [Board]
callNumber n = mapM (updateBoard n)

checkWin :: Board -> Bool
checkWin b = any isBingo $ rows b ++ cols b

rows :: Board -> [Vector Cell]
rows b = map ((flip getRow) b) [1..(nrows b)]

cols :: Board -> [Vector Cell]
cols b = map ((flip getCol) b) [1..(ncols b)]

isBingo :: Vector Cell -> Bool
isBingo v = isBingo' $ V.toList v

isBingo' :: [Cell] -> Bool
isBingo' [M _, M _, M _, M _, M _] = True
isBingo' _ = False

updateBoard :: Int -> Board -> Game Board
updateBoard n b
  | checkWin b' = Left (b', n)
  | otherwise = Right b'
  where b' = fmap (mark n) b

mark :: Int -> Cell -> Cell
mark _  (M n) = M n
mark n' (U n) = if n == n' then M n' else U n
