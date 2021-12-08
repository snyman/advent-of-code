#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers

import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import Data.List

main :: IO ()
main = do
  lines <- fmap lines getContents
  let signals = map parseSignals lines
  let outputs = map computeOutput signals
  putStrLn $ show $ sum outputs

parseSignals :: String -> ([String], [String])
parseSignals s = (patterns s, outputs s)
    where patterns = words . (!!0) . splitOn "|"
          outputs = words . (!!1) . splitOn "|"

countDigits :: [[String]] -> Int
countDigits signals = length $ catMaybes $ concatMap (\ss -> map parseScrambledDigit ss) signals

parseScrambledDigit :: String -> Maybe Int
parseScrambledDigit [_,_] = Just 1
parseScrambledDigit [_,_,_,_] = Just 4
parseScrambledDigit [_,_,_] = Just 7
parseScrambledDigit [_,_,_,_,_,_,_] = Just 8
parseScrambledDigit _ = Nothing

computeOutput :: ([String], [String]) -> Int
computeOutput (patterns, outputs) = read $ concatMap (show . parseDigit) descrambledOutputs
    where descrambledOutputs = unscrambleSignals wireMapping outputs
          wireMapping = decodeWires patterns

parseDigit :: String -> Int
parseDigit "abcefg" = 0
parseDigit "cf" = 1
parseDigit "acdeg" = 2
parseDigit "acdfg" = 3
parseDigit "bcdf" = 4
parseDigit "abdfg" = 5
parseDigit "abdefg" = 6
parseDigit "acf" = 7
parseDigit "abcdefg" = 8
parseDigit "abcdfg" = 9

unscrambleSignals :: M.Map Char Char -> [String] -> [String]
unscrambleSignals wireMapping words = map (sort . descramble) words
    where descramble = map (wireMapping M.!)

decodeWires :: [String] -> M.Map Char Char
decodeWires words = M.fromList $ map (decodeWire words) ['a'..'g']

decodeWire :: [String] -> Char -> (Char, Char)
decodeWire words letter =
    case wordsWithLetter letter words of
        8 -> decideAOrC letter words
        6 -> (letter, 'b')
        7 -> decideDOrG letter words
        4 -> (letter, 'e')
        9 -> (letter, 'f')

decideAOrC :: Char -> [String] -> (Char, Char)
decideAOrC letter words = if letter `elem` one then (letter, 'c') else (letter, 'a')
    where one = fromJust $ find ((==2) . length) words

decideDOrG :: Char -> [String] -> (Char, Char)
decideDOrG letter words = if letter `elem` four then (letter, 'd') else (letter, 'g')
    where four = fromJust $ find ((==4) . length) words

wordsWithLetter :: Char -> [String] -> Int
wordsWithLetter l s = length $ filter (elem l) s

-- a - 8
-- b - 6
-- c - 8
-- d - 7
-- e - 4
-- f - 9
-- g - 7

-- a is not in 1 but c is
-- d is in 4 and g isn't
