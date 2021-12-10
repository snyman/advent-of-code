#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array

import Data.List
import Control.Monad
import Data.Either

main :: IO ()
main = do
  input <- fmap lines getContents
  putStrLn $ show $ scoreErrors input
  putStrLn $ show $ scoreChunks input

type ParseResult = Either Int

parseChunks :: String -> ParseResult [Char]
parseChunks = foldM pushBracket []

pushBracket :: [Char] -> Char -> ParseResult [Char]
pushBracket ('(':s) ')' = Right s
pushBracket ('[':s) ']' = Right s
pushBracket ('{':s) '}' = Right s
pushBracket ('<':s) '>' = Right s
pushBracket s '(' = Right $ '(':s
pushBracket s '[' = Right $ '[':s
pushBracket s '{' = Right $ '{':s
pushBracket s '<' = Right $ '<':s
pushBracket _ c = Left $ errorScore c

errorScore :: Char -> Int
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137

scoreErrors :: [String] -> Int
scoreErrors = sum . lefts . (map parseChunks)

scoreChunk :: [Char] -> Int
scoreChunk s = foldl addBracketScore 0 s
  where addBracketScore score c = score * 5 + completionScore c

completionScore :: Char -> Int
completionScore '(' = 1
completionScore '[' = 2
completionScore '{' = 3
completionScore '<' = 4

scoreChunks :: [String] -> Int
scoreChunks ss = scores !! mid
  where
    scores = sort $ map scoreChunk $ rights $ map parseChunks ss
    mid = floor $ (fromIntegral $ length scores) / 2
