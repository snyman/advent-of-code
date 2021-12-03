#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script

main :: IO ()
main = do
  diagnostics <- getContents >>= return . lines
  let gamma = gammaRate $ map bits diagnostics
  let epsilon = epsilonRate gamma
  putStrLn $ show gamma
  putStrLn $ show epsilon
  putStrLn $ show ((toDecimal gamma) * (toDecimal epsilon))

bits :: String -> [Bool]
bits ('1':bs) = True:bits bs
bits ('0':bs) = False:bits bs
bits [] = []

toDecimal :: [Bool] -> Int
toDecimal = foldl (\n b -> (n * 2) + (if b then 1 else 0)) 0

gammaRate :: [[Bool]] -> [Bool]
gammaRate = commonBits . (foldl combine $ repeat 0)

epsilonRate :: [Bool] -> [Bool]
epsilonRate = map not

combine :: [Int] -> [Bool] -> [Int]
combine ns bs = map (\(n, b) -> if b then n + 1 else n - 1) $ zip ns bs

commonBits :: [Int] -> [Bool]
commonBits = map (>0)
