#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array --package PSQueue --package mtl

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State


main :: IO ()
main = do
  scanners <- fmap (parseScanners . lines) getContents
  putStrLn $ show scanners
  let lengthMap = distancesPerScanner scanners
  putStrLn $ show $ possibleMatches $ M.assocs $ lengthMap
  putStrLn $ show $ commonPoints (scanners !! 0) (scanners !! 1)
  putStrLn $ show $ findTransform $ commonPoints (scanners !! 0) (scanners !! 1)
  putStrLn $ show $ createContext scanners
  let allCoords = unifyCoords scanners
  putStrLn $ show $ allCoords
  putStrLn $ show $ S.size allCoords

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

type Coord = (Int, Int, Int)
type Scanner = [Coord]

parseBeacon :: String -> Coord
parseBeacon s = (read x, read y, read z)
  where (x:y:z:[]) = splitOn "," s

parseScanners :: [String] -> [Scanner]
parseScanners [] = []
parseScanners ("":s) = parseScanners s
parseScanners (('-':'-':'-':_):s) = (map parseBeacon beacons):parseScanners rem
  where (beacons, rem) = span ((>0) . length) s

distancesPerScanner :: [Scanner] -> M.Map Int (S.Set Double)
distancesPerScanner ss = M.fromList $ zip [0..] $ map (M.keysSet . pairwiseDistances) ss

pairwiseDistances :: Scanner -> M.Map Double (S.Set Coord)
pairwiseDistances s = M.fromListWith (S.union) $ do
  first <- s
  second <- s
  if first < second then return $ (distance first second, S.fromAscList [first, second]) else []

distance :: Coord -> Coord -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

possibleMatches :: [(Int, S.Set Double)] -> M.Map Int (S.Set Int)
possibleMatches lengths = M.fromListWith S.union $ do
  (n1, n1Lengths) <- lengths
  (n2, n2Lengths) <- lengths
  let matchingLengths = S.size $ n1Lengths `S.intersection` n2Lengths
  if n1 < n2 && matchingLengths >= 66 then [(n1, S.singleton n2), (n2, S.singleton n1)] else []

commonPoints :: Scanner -> Scanner -> [(Coord, Coord)]
commonPoints a b = zip (sortedByDistanceSum commonAs) (sortedByDistanceSum commonBs)
  where
    aDist = pairwiseDistances a
    bDist = pairwiseDistances b
    commonDist = S.toList $ M.keysSet aDist `S.intersection` M.keysSet bDist
    commonCoords distances = map fst $ filter ((>=11) . snd) $ M.assocs $ M.fromListWith (+) $ zip (concatMap (S.toList . (distances M.!)) commonDist) [1,1..]
    commonAs = commonCoords aDist
    commonBs = commonCoords bDist
    distanceSum coords coord = sum $ map (distance coord) coords
    sortedByDistanceSum coords = sortOn (distanceSum coords) coords

data CT = PX | PY | PZ | NX | NY | NZ deriving (Eq, Show, Ord)

neg :: CT -> CT
neg PX = NX
neg PY = NY
neg PZ = NZ
neg NX = PX
neg NY = PY
neg NZ = PZ

type Transform = (CT, CT, CT)

pick :: CT -> Coord -> Int
pick PX (x, _, _) = x
pick NX (x, _, _) = -x
pick PY (_, y, _) = y
pick NY (_, y, _) = -y
pick PZ (_, _, z) = z
pick NZ (_, _, z) = -z

transform :: Transform -> Coord -> Coord
transform (xt, yt, zt) c = (pick xt c, pick yt c, pick zt c)

data Rot = R0 | R90 | R180 | R270

rotate :: Rot -> (CT, CT) -> (CT, CT)
rotate R0 (a, b) = (a, b)
rotate R90 (a, b) = (b, neg a)
rotate R180 (a, b) = (neg a, neg b)
rotate R270 (a, b) = (neg b, a)

rotateAroundX :: Rot -> Transform -> Transform
rotateAroundX deg (x, y, z) = (x, a, b)
  where (a, b) = rotate deg (y, z)

rotateAroundY :: Rot -> Transform -> Transform
rotateAroundY deg (x, y, z) = (a, y, b)
  where (a, b) = rotate deg (x, z)

rotateAroundZ :: Rot -> Transform -> Transform
rotateAroundZ deg (x, y, z) = (a, b, z)
  where (a, b) = rotate deg (x, y)

allTransforms :: [Transform]
allTransforms = S.toList $ S.fromList $ do
  let rot = [R0, R90, R180, R270]
  xRot <- rot
  yRot <- rot
  zRot <- rot
  return $ rotateAroundX xRot $ rotateAroundY yRot $ rotateAroundZ zRot $ (PX, PY, PZ)

diff :: Coord -> Coord -> Coord
diff (x, y, z) (x', y', z') = (x - x', y - y', z - z')

plus :: Coord -> Coord -> Coord
plus (x, y, z) (x', y', z') = (x + x', y + y', z + z')

findTransform :: [(Coord, Coord)] -> Maybe Transform
findTransform pairs = find ((==1) . S.size . diffs) allTransforms
  where
    diffs tx = S.fromList $ map (\(c1, c2) -> c1 `diff` (transform tx c2)) pairs

unifyCoords :: [Scanner] -> S.Set Coord
unifyCoords ss = foldl foldCoords S.empty $ zip [0..] $ map S.fromList ss
  where
    scannerMap = M.fromList $ zip [0..] ss
    visited = S.singleton 0
    unvisited = S.fromList [1..(length ss - 1)]
    transforms = M.singleton 0 id
    relation = possibleMatches $ M.assocs $ distancesPerScanner ss
    startCtx = Context visited unvisited relation transforms
    ctx@(Context _ _ _ finalTxs) = execState (Main.traverse $ mergeScanner ss) startCtx
    foldCoords coords (idx, scn) = coords `S.union` (S.map (finalTxs M.! idx) scn)

createContext :: [Scanner] -> Context
createContext ss = startCtx
  where
    scannerMap = M.fromList $ zip [0..] ss
    visited = S.singleton 0
    unvisited = S.fromList [1..(length ss - 1)]
    transforms = M.singleton 0 id
    relation = possibleMatches $ M.assocs $ distancesPerScanner ss
    startCtx = Context visited unvisited relation transforms

mergeScanner :: [Scanner] -> Int -> Int -> Maybe (Coord -> Coord)
mergeScanner ss n1 n2 = composedTx
  where
    s1 = ss !! n1
    s2 = ss !! n2
    common = commonPoints s1 s2
    rotTx = findTransform common
    (first, second) = head common
    beaconOffset = do
      rotation <- rotTx
      let offset = first `diff` (transform rotation second)
      return $ plus offset
    composedTx = do
      rotation <- rotTx
      offset <- beaconOffset
      return $ offset . (transform rotation)

data Context = Context {
  visited :: S.Set Int,
  unvisited :: S.Set Int,
  relation :: M.Map Int (S.Set Int),
  transforms :: M.Map Int (Coord -> Coord)
  }

instance Show Context where
  show (Context v u r _) = show ("Context", v, u, r)

traverse :: (Int -> Int -> Maybe (Coord -> Coord)) -> State Context ()
traverse visit = do
  (Context visited unvisited relation transforms) <- get
  if S.size unvisited > 0 then do
    nextPairs <- gets nextPairs
    let (v, u, tx) = head $ mapMaybe tryVisit nextPairs
    let vTx = transforms M.! v
    let uTx = vTx . tx
    put $ Context (u `S.insert` visited) (u `S.delete` unvisited) relation (M.insert u uTx transforms)
    Main.traverse visit
  else return ()
  where
    tryVisit (v, u) = visit v u >>= (\tx -> Just (v, u, tx))

nextPairs :: Context -> [(Int, Int)]
nextPairs (Context visited unvisited rel _) = do
  v <- S.toList visited
  u <- S.toList unvisited
  if u `S.member` (rel M.! v) then [(v, u)] else []

