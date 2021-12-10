#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array

import Data.List
import Control.Monad
import Data.Either

main :: IO ()
main = do
  input <- fmap lines getContents
  putStrLn $ show $ inputScore input

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

inputScore :: [String] -> Int
inputScore = sum . lefts . (map parseChunks)
