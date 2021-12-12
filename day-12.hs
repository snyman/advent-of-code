#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Char

main :: IO ()
main = do
  caves <- fmap (parseCaves . lines) getContents
  putStrLn $ show caves
  putStrLn $ show $ length $ traverseCaves caves [] "start"

type Caves = M.Map String [String]
type Path = [String]

parseEdge :: Caves -> String -> Caves
parseEdge c s = M.insertWith (++) a [b] $ M.insertWith (++) b [a] c
  where (a:b:_) = splitOn "-" s

parseCaves :: [String] -> Caves
parseCaves = foldl parseEdge M.empty

neighbours :: Caves -> String -> [String]
neighbours c s = M.findWithDefault [] s c

isBig :: String -> Bool
isBig = all isUpper

isValidStep :: Path -> String -> Bool
isValidStep p s
  | isBig s = True
  | otherwise = notElem s p

traverseCaves :: Caves -> Path -> String -> [Path]
traverseCaves _ p "end" = ["end":p]
traverseCaves c p s = foldl addPaths [] $ filter (isValidStep p) nodes
  where
    nodes = neighbours c s
    addPaths ps s' = ps ++ traverseCaves c (s:p) s'
