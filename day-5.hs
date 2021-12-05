#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers

import Data.List.Split
import Data.Char
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  ventLines <- fmap lines getContents
  let vents = map parseVent ventLines
  let orthogonalVents = filter isOrthogonal vents
  mapM_ putStrLn $ map show orthogonalVents
  let insPts = intersections $ allPoints orthogonalVents
  putStrLn $ show insPts
  putStrLn $ show $ length insPts

data Vent = Vent {x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int} deriving (Show, Eq)

parseVent :: String -> Vent
parseVent s = v $ map read $ wordsBy (not . isDigit) s
  where v [x1, y1, x2, y2] = Vent x1 y1 x2 y2

isOrthogonal :: Vent -> Bool
isOrthogonal v = (x1 v == x2 v) || (y1 v == y2 v)

points :: Vent -> [(Int, Int)]
points v
  | xa == xb && ya <= yb = zip [xa,xb..] [ya..yb]
  | xa == xb && ya > yb = zip [xa,xb..] [yb..ya]
  | xa <= xb && ya == yb = zip [xa..xb] [ya,yb..]
  | xa > xb && ya == yb = zip [xb..xa] [ya,yb..]
  where xa = x1 v
        xb = x2 v
        ya = y1 v
        yb = y2 v

intersections :: M.Map (Int, Int) Int -> [(Int, Int)]
intersections m = M.foldlWithKey addIntersection [] m
  where addIntersection pts pt 1 = pts
        addIntersection pts pt _ = pt:pts

allPoints :: [Vent] -> M.Map (Int, Int) Int
allPoints vs = foldl addToMap M.empty $ concatMap points vs
  where addToMap m pt = M.alter update pt m
        update Nothing = Just 1
        update (Just x) = Just $ x + 1
