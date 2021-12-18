#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array --package PSQueue --package mtl

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Control.Monad.State

testExplosions = "\
\[[[[[9,8],1],2],3],4]\n\
\[7,[6,[5,[4,[3,2]]]]]\n\
\[[6,[5,[4,[3,2]]]],1]\n\
\[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"

testSplits = "\
\[10,0]\n\
\[0,[11,0]]\n\
\[0,[0,11]]\n\
\[0,[0,0]]"

testMagnitudes = "\
\[[1,2],[[3,4],5]]\n\
\[[[[0,7],4],[[7,8],[6,0]]],[8,1]]\n\
\[[[[1,1],[2,2]],[3,3]],[4,4]]\n\
\[[[[3,0],[5,3]],[4,4]],[5,5]]\n\
\[[[[5,0],[7,4]],[5,5]],[6,6]]\n\
\[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

main :: IO ()
main = do
  putStrLn "Explosions"
  mapM_ (putStrLn . show . explode . zipper) $ parseExpressions testExplosions
  putStrLn "Splits"
  mapM_ (putStrLn . show . sfSplit . zipper) $ parseExpressions testSplits
  putStrLn "Reduce"
  putStrLn $ show $ reduce $ head $ parseExpressions "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
  putStrLn "Add"
  putStrLn $ show $ addAll $ parseExpressions "[[[[4,3],4],4],[7,[[8,4],9]]]\n[1,1]"
  putStrLn $ show $ addAll $ parseExpressions "[1,1]\n[2,2]\n[3,3]\n[4,4]"
  putStrLn $ show $ addAll $ parseExpressions "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]"
  putStrLn $ show $ addAll $ parseExpressions "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]"
  putStrLn "Magnitude"
  mapM_ (putStrLn . show . magnitude) $ parseExpressions testMagnitudes
  putStrLn "Input"
  nums <- fmap parseExpressions getContents
  let finalSum = addAll nums
  putStrLn $ show $ finalSum
  putStrLn $ show $ magnitude finalSum
  putStrLn "Largest Sum of Pairs by Magnitude"
  putStrLn $ show $ largestPairMagnitude nums

biSplit :: Eq a => [a] -> [a] -> ([a], [a])
biSplit delim s = (a, concat (b:c))
  where (a:b:c) = splitOn delim s

mapFirst :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapFirst _ _ [] = []
mapFirst pred act (a:as)
  | pred a = act a:as
  | otherwise = a:mapFirst pred act as

cmpFst :: Ord a => (a, b) -> (a, b) -> Ordering
cmpFst (a1, _) (a2, _) = a1 `compare` a2

cmpSnd :: Ord b => (a, b) -> (a, b) -> Ordering
cmpSnd (_, b1) (_, b2) = b1 `compare` b2

data Sf = Pair Sf Sf | Leaf Int deriving(Eq)

instance Show Sf where
  show (Leaf n) = show n
  show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

data Crumb = L Sf | R Sf

isLeft :: Crumb -> Bool
isLeft (L _) = True
isLeft _ = False

isRight :: Crumb -> Bool
isRight = not . isLeft

cmap :: (Sf -> Sf) -> Crumb -> Crumb
cmap act (L sf) = L $ act sf
cmap act (R sf) = R $ act sf

type Zipper = (Sf, [Crumb])

goLeft :: Zipper -> Zipper
goLeft (Pair l r, cs) = (l, L r:cs)

goRight :: Zipper -> Zipper
goRight (Pair l r, cs) = (r, R l:cs)

goUp :: Zipper -> Zipper
goUp (l, L r:cs) = (Pair l r, cs)
goUp (r, R l:cs) = (Pair l r, cs)

zipper :: Sf -> Zipper
zipper sf@(Pair l r) = (sf, [])

zipUp :: Zipper -> Sf
zipUp (sf, []) = sf
zipUp z = zipUp $ goUp z

zipUpTo :: (Crumb -> Bool) -> Zipper -> Maybe Zipper
zipUpTo _ (_, []) = Nothing
zipUpTo pred z@(sf, c:_)
  | pred c = Just $ goUp z
  | otherwise = zipUpTo pred $ goUp z

type Parser = State String

pull :: Int -> Parser String
pull n = do
  prefix <- gets $ take n
  modify $ drop n
  return prefix

pullWhile :: (Char -> Bool) -> Parser String
pullWhile predicate = do
  (prefix, remainder) <- gets $ span predicate
  put remainder
  return prefix

parseExpression :: Parser Sf
parseExpression = do
  first <- gets head
  case first of
    '[' -> do
      modify tail
      left <- parseExpression
      modify tail
      right <- parseExpression
      modify tail
      return $ Pair left right
    _ -> do
      val <- fmap read $ pullWhile (`notElem` ",]")
      return $ Leaf val

parseExpressions :: String -> [Sf]
parseExpressions s = map (evalState parseExpression) $ lines s

explode :: Zipper -> Sf
explode (Pair (Leaf l) (Leaf r), cs@(a:b:c:d:_)) = zipUp explodedZip
  where
    zerodZip = (Leaf 0, cs)
    addedLeftZip = modifyLeft (addRight l) zerodZip
    explodedZip = modifyRight (addLeft r) addedLeftZip
explode z@(Leaf n, _) =
  case nextRight of
    Just z' -> explode $ goRight z'
    Nothing -> zipUp z
  where nextRight = zipUpTo isLeft z
explode z = explode $ goLeft z

modifyLeft :: (Sf -> Sf) -> Zipper -> Zipper
modifyLeft act (sf, cs) = (sf, mapFirst isRight (cmap act) cs)

modifyRight :: (Sf -> Sf) -> Zipper -> Zipper
modifyRight act (sf, cs) = (sf, mapFirst isLeft (cmap act) cs)

addLeft :: Int -> Sf -> Sf
addLeft n (Leaf m) = Leaf $ n + m
addLeft n (Pair l r) = Pair (addLeft n l) r

addRight :: Int -> Sf -> Sf
addRight n (Leaf m) = Leaf $ n + m
addRight n (Pair l r) = Pair l (addRight n r)

sfSplit :: Zipper -> Sf
sfSplit z@(Leaf n, cs)
  | n >= 10 = let half = fromIntegral n / 2 in zipUp $ (Pair (Leaf $ floor half) (Leaf $ ceiling half), cs)
  | otherwise =
    let nextRight = zipUpTo isLeft z in
    case nextRight of
      Just z' -> sfSplit $ goRight z'
      Nothing -> zipUp z
sfSplit z = sfSplit $ goLeft z

reduce :: Sf -> Sf
reduce sf
  | sf /= exploded = reduce exploded
  | sf /= split = reduce split
  | otherwise = sf
  where
    zip = zipper sf
    exploded = explode zip
    split = sfSplit zip

add :: Sf -> Sf -> Sf
add l r = reduce $ Pair l r

addAll :: [Sf] -> Sf
addAll = foldl1 add

magnitude :: Sf -> Int
magnitude (Leaf n) = n
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

largestPairMagnitude :: [Sf] -> (Int, (Sf, Sf))
largestPairMagnitude sfs = maximumBy cmpFst $ do
  l <- sfs
  r <- sfs
  if l /= r then return (magnitude $ l `add` r, (l, r)) else []
