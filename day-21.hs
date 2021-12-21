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
  putStrLn "---------------------------"
  let qGameStart = qgame (p1, p2)
  let (qFinalScore, qGameEnd) = runState playQuantumGame qGameStart
  print qGameEnd
  print qFinalScore

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

data Player = P { pos :: Int, score :: Int } deriving (Eq, Show, Ord)
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

data QGame = QG { qPlayers :: M.Map (Player, Player) Int, wins :: (Int, Int) } deriving (Eq, Show)

qgame :: (Player, Player) -> QGame
qgame ps = QG (M.singleton ps 1) (0, 0)

quantumRoll :: [(Int, Int)]
quantumRoll = M.assocs $ M.fromListWith (+) $ do
  let faces = [1, 2, 3]
  roll1 <- faces
  roll2 <- faces
  roll3 <- faces
  return $ (roll1 + roll2 + roll3, 1)

quantumMove :: M.Map (Player, Player) Int -> M.Map (Player, Player) Int
quantumMove pMap = M.fromListWith (+) $ do
  ((p1, p2), n1) <- M.assocs pMap
  (move, n2) <- quantumRoll
  return ((p2, movePlayer p1 move), n1 * n2)

quantumTurn :: State QGame ()
quantumTurn = do
  players <- gets qPlayers
  (w1, w2) <- gets wins
  let players' = quantumMove players
  modify $ \g -> g { qPlayers = players', wins = (w2, w1) }
  collapseWins

collapseWins :: State QGame ()
collapseWins = do
  players <- gets qPlayers
  let (finished, ongoing) = M.partitionWithKey hasWinner players
  mapM_ addWins $ M.assocs finished
  modify $ \g -> g { qPlayers = ongoing }

hasWinner :: (Player, Player) -> Int -> Bool
hasWinner (p1, p2) _ = score p1 >= 21 || score p2 >= 21

addWins :: ((Player, Player), Int) -> State QGame ()
addWins ((p1, p2), n) = do
  (w1, w2) <- gets wins
  let wins' = if score p1 >= 1000 then (w1 + n, w2) else (w1, w2 + n)
  modify $ \g -> g { wins = wins' }

qIsDone :: QGame -> Bool
qIsDone = (==0) . M.size . qPlayers

qFinalScore :: QGame -> Int
qFinalScore (QG _ (w1, w2)) = max w1 w2

playQuantumGame :: State QGame Int
playQuantumGame = do
  done <- gets qIsDone
  if done then gets qFinalScore else do
    quantumTurn
    playQuantumGame
