#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array

import Data.List
import Data.List.Split
import qualified Data.Map as M

main :: IO ()
main = do
  (init, rules) <- fmap parseInput getContents
  putStrLn init
  mapM_ (putStrLn . show) $ M.toList rules
  showStep init rules 1
  showStep init rules 2
  showStep init rules 3
  showStep init rules 4
  showStep init rules 10

showStep :: String -> Rules -> Int -> IO ()
showStep init rules n = do
  let genN = generate rules n init
  putStrLn $ genN
  let mm@((minC, minN), (maxC, maxN)) = minMax genN
  putStrLn $ show $ mm
  putStrLn $ show $ maxN - minN


biSplit :: Eq a => [a] -> [a] -> ([a], [a])
biSplit delim s = (a, concat (b:c))
  where (a:b:c) = splitOn delim s

type Rules = M.Map String String

parseInput :: String -> (String, Rules)
parseInput ss = (init, parseRules rules)
  where (init:_:rules) = lines ss

parseRules :: [String] -> Rules
parseRules = M.fromList . map (biSplit " -> ")

generate :: Rules -> Int -> String -> String
generate r 0 s = s
generate r n s@[a, b] = generate r (n-1) [a, c, b]
  where [c] = r M.! s
generate r n s = foldr concatGen "" $ pairwise s
  where
    concatGen p "" = generate r n p
    concatGen p (h:gen) = (generate r n p) ++ gen

pairwise :: String -> [String]
pairwise [a,b] = [[a,b]]
pairwise (a:b:c) = [a,b]:pairwise (b:c)

minMax :: String -> ((Char, Int), (Char, Int))
minMax s = (min, max)
  where
    counts = M.assocs $ M.fromListWith (+) $ zip s $ repeat 1
    min = minimumBy (\(_, n) (_, n') -> compare n n') counts
    max = maximumBy (\(_, n) (_, n') -> compare n n') counts
