#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script

data BinTree = Leaf | Node Int BinTree BinTree deriving Show

main :: IO ()
main = do
  diagnostics <- getContents >>= return . lines
  let gamma = gammaRate $ map bits diagnostics
  let epsilon = epsilonRate gamma
  putStrLn $ show gamma
  putStrLn $ show epsilon
  putStrLn $ show ((toDecimal gamma) * (toDecimal epsilon))
  let popTree = buildTree $ map bits diagnostics
  let oxy = oxygenRate popTree
  let co2 = co2Rate popTree
  putStrLn $ show $ oxy
  putStrLn $ show $ co2
  putStrLn $ show ((toDecimal oxy) * (toDecimal co2))

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

buildTree :: [[Bool]] -> BinTree
buildTree = foldl add Leaf

add :: BinTree -> [Bool] -> BinTree
add (Node n ts fs) (True:bs) = Node (n + 1) (ts `add` bs) fs
add (Node n ts fs) (False:bs) = Node (n + 1) ts (fs `add` bs)
add (Node n ts fs) [] = Node (n + 1) ts fs
add Leaf bs = (Node 0 Leaf Leaf) `add` bs

size :: BinTree -> Int
size (Node n _ _) = n
size Leaf = 0

oxygenRate :: BinTree -> [Bool]
oxygenRate (Node _ Leaf Leaf) = []
oxygenRate (Node _ t f)
  | size t >= size f = True:oxygenRate t
  | otherwise       = False:oxygenRate f
oxygenRate Leaf = []

co2Rate :: BinTree -> [Bool]
co2Rate (Node _ Leaf Leaf) = []
co2Rate (Node _ t Leaf) = True:co2Rate t
co2Rate (Node _ Leaf f) = False:co2Rate f
co2Rate (Node _ t f)
  | size t < size f = True:co2Rate t
  | otherwise       = False:co2Rate f
co2Rate Leaf = []
