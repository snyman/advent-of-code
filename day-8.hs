#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split

import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  lines <- fmap lines getContents
  let outputs = map parseSignals lines
  putStrLn $ show $ outputs
  putStrLn $ show $ countDigits outputs

parseSignals :: String -> [String]
parseSignals = words . (!!1) . splitOn "|"

countDigits :: [[String]] -> Int
countDigits signals = length $ catMaybes $ concatMap (\ss -> map parseDigit ss) signals

parseDigit :: String -> Maybe Int
parseDigit [_,_] = Just 1
parseDigit [_,_,_,_] = Just 4
parseDigit [_,_,_] = Just 7
parseDigit [_,_,_,_,_,_,_] = Just 8
parseDigit _ = Nothing
