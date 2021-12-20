#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array --package PSQueue --package mtl

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Array

main :: IO ()
main = do
  (ehc, bitmap) <- fmap parseInput getContents
  putStrLn $ show ehc
  let enhance = enhanceBitmap ehc
  let bitmap' = composeN enhance 2 bitmap
  printBitmap bitmap
  putStrLn ""
  printBitmap bitmap'
  print $ count bitmap'
  putStrLn ""
  let bitmap'' = composeN enhance 48 bitmap'
  printBitmap bitmap''
  print $ count bitmap''

composeN :: (a -> a) -> Int -> a -> a
composeN _ 0 a = a
composeN fn n a = composeN fn (n-1) (fn a)

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

newtype Point = Point { pt :: (Int, Int) } deriving (Eq, Show)
instance Ord Point where
  compare (Point (x1, y1)) (Point (x2, y2)) = compare (y1, x1) (y2, x2)

data LED = On | Off deriving (Eq)

instance Show LED where
  show On = "#"
  show Off = "."

led :: Char -> LED
led '#' = On
led '.' = Off

type Enhance = Array Int LED
data Bitmap = Bmp { points :: S.Set Point, bmpBounds :: (Point, Point), def :: LED }

parseInput :: String -> (Enhance, Bitmap)
parseInput s = (parseEnhance enhanceStr, parseBitmap bmpStrs)
  where
    ss = lines s
    enhanceStr = head ss
    bmpStrs = drop 2 ss

parseEnhance :: String -> Enhance
parseEnhance s = array (0, 511) $ zip [0..] $ map led s

parseBitmap :: [String] -> Bitmap
parseBitmap rows = Bmp pts (Point (0, 0), Point (colCount - 1, rowCount - 1)) Off
  where
    pts = S.fromList [Point (x,y) | (row, y) <- zip rows [0..], (l, x) <- zip (map led row) [0..], l == On]
    rowCount = length rows
    colCount = length $ head rows

inside :: Point -> (Point, Point) -> Bool
inside (Point (x, y)) (Point (minX, minY), Point (maxX, maxY)) = x >= minX && x <= maxX && y >= minY && y <= maxY

getLed :: Bitmap -> Point -> LED
getLed (Bmp pts bnds def) pt
  | pt `S.member` pts = On
  | pt `inside` bnds = Off
  | otherwise = def

printBitmap :: Bitmap -> IO ()
printBitmap bmp = printBitmap' 0 bmp

printBitmap' :: Int -> Bitmap -> IO ()
printBitmap' n bmp = mapM_ printRow [minY-n..maxY+n]
  where
    (Point (minX, minY), Point (maxX, maxY)) = bmpBounds bmp
    printRow y = do
      mapM_ (putStr . show . (getLed bmp)) [Point (x, y) | x <- [minX-n..maxX+n]]
      putStrLn ""

toInt :: [LED] -> Int
toInt = foldl (\n led -> n * 2 + (bitVal led)) 0
  where
    bitVal On = 1
    bitVal Off = 0

enhancePoint :: Enhance -> Bitmap -> Point -> LED
enhancePoint ehc bmp (Point (x, y)) = ehc ! idx
  where
    neighbours = [getLed bmp (Point (x, y)) | y <- [y-1, y, y+1], x <- [x-1, x, x+1]]
    idx = toInt neighbours

enhanceBitmap :: Enhance -> Bitmap -> Bitmap
enhanceBitmap ehc bmp = Bmp enhancedPoints newBounds newDef
  where
    newDef = ehc ! (toInt $ replicate 9 $ def bmp)
    (Point (minX, minY), Point (maxX, maxY)) = bmpBounds bmp
    newBounds = (Point (minX - 1, minY - 1), Point (maxX + 1, maxY + 1))
    enhancedPoints = S.fromList [Point (x, y) | x <- [minX - 1..maxX + 1], y <- [minY - 1..maxY + 1], On == enhancePoint ehc bmp (Point (x, y))]

count :: Bitmap -> Int
count (Bmp pts _ def)
  | def == On = maxBound
  | otherwise = S.size pts
