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
  [p1, p2] <- fmap parseInput getContents
  let gameStart = game (p1, p2)
  let (finalScore, gameEnd) = runState playGame gameStart 
  print gameEnd
  print finalScore

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

data Player = P { pos :: Int, score :: Int } deriving (Eq, Show)
data Game = G { players :: (Player, Player), dice :: [Int], turns :: Int } deriving (Eq)

instance Show Game where
  show (G ps ds ts) = show (ps, take 10 ds, ts)

parseInput :: String -> [Player]
parseInput = (map parsePlayer) . lines

parsePlayer :: String -> Player
parsePlayer s = P pos 0
  where
    pos = read $ last $ words s

game :: (Player, Player) -> Game
game ps = G ps deterministicDice 0

deterministicDice :: [Int]
deterministicDice = cycle [1..100]

movePlayer :: Player -> Int -> Player
movePlayer (P pos score) move = P newPos (score + newPos)
  where
    newPos = (((pos - 1) + move) `mod` 10) + 1

rollDice :: State Game Int
rollDice = do
  ds <- gets dice
  let rolls = take 3 ds
  let ds' = drop 3 ds
  modify $ \g -> g { dice = ds' }
  return $ sum rolls

takeTurn :: State Game ()
takeTurn = do
  turn <- gets turns
  (current, next) <- gets players
  rolls <- rollDice
  let current' = movePlayer current rolls
  modify $ \g -> g { players = (next, current'), turns = turn + 1 }

isDone :: Game -> Bool
isDone (G (p1, p2) _ _) = score p1 >= 1000 || score p2 >= 1000

finalScore :: Game -> Int
finalScore (G (p1, p2) _ turn) = losingScore * turn * 3
  where
    losingScore = min (score p1) (score p2)

playGame :: State Game Int
playGame = do
  done <- gets isDone
  if done then gets finalScore else do
    takeTurn
    playGame
