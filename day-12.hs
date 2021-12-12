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
  -- mapM_ (putStrLn . show) $ traverseCaves caves ([], False) "start"
  putStrLn $ show $ length $ traverseCaves caves ([], False) "start"

type Caves = M.Map String [String]
type Path = ([String], Bool)

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
isValidStep _ "start" = False
isValidStep (p, alreadyVisited) s
  | isBig s = True
  | notElem s p = True
  | otherwise = not alreadyVisited

addToPath :: String -> Path -> Path
addToPath s (p, alreadyVisited)
  | isBig s || notElem s p = (s:p, alreadyVisited)
  | otherwise = (s:p, True)

traverseCaves :: Caves -> Path -> String -> [Path]
traverseCaves _ p "end" = ["end" `addToPath` p]
traverseCaves c p s = foldl addPaths [] $ filter (isValidStep newPath) nodes
  where
    newPath = s `addToPath` p
    nodes = neighbours c s
    addPaths ps s' = ps ++ traverseCaves c newPath s'
