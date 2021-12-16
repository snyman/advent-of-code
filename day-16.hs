#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script --package split --package containers --package array --package PSQueue --package mtl

import Data.List
import Data.List.Split
import Data.Array.IArray
import qualified Data.Map as M
import Control.Monad.State

testConst = "D2FE28"
testSubPacketByLength = "38006F45291200"
testSubPacketByCount = "EE00D40C823060"
testWithSum16 = "8A004A801A8002F478"
testWithSum12 = "620080001611562C8802118E34"
testWithSum23 = "C0015000016115A2E0802F182340"
testWithSum31 = "A0016C880162017C3686B18A3D4780"

main :: IO ()
main = do
  run testConst
  run testSubPacketByLength
  run testSubPacketByCount
  test testWithSum16 16
  test testWithSum12 12
  test testWithSum23 23
  test testWithSum31 31
  input <- getContents
  run input
  return ()

test :: String -> Int -> IO ()
test s n = do
  sumVersion <- run s
  if n == sumVersion then putStrLn "Passed" else putStrLn ("Failed: (Expected, Actual)" ++ (show (n, sumVersion)))

run :: String -> IO Int
run s = do
  let bits = parseHex s
  putStrLn $ show bits
  let (packet, remainder) = runState parsePacket bits
  putStrLn $ show $ (packet, remainder)
  let sumVersion = sumVersions packet
  putStrLn $ show $ sumVersion
  return sumVersion

biSplit :: Eq a => [a] -> [a] -> ([a], [a])
biSplit delim s = (a, concat (b:c))
  where (a:b:c) = splitOn delim s

data Bit = O | I deriving(Eq, Show)

parseHex :: String -> [Bit]
parseHex = concatMap parseHexC

parseHexC :: Char -> [Bit]
parseHexC '0' = [O,O,O,O]
parseHexC '1' = [O,O,O,I]
parseHexC '2' = [O,O,I,O]
parseHexC '3' = [O,O,I,I]
parseHexC '4' = [O,I,O,O]
parseHexC '5' = [O,I,O,I]
parseHexC '6' = [O,I,I,O]
parseHexC '7' = [O,I,I,I]
parseHexC '8' = [I,O,O,O]
parseHexC '9' = [I,O,O,I]
parseHexC 'A' = [I,O,I,O]
parseHexC 'B' = [I,O,I,I]
parseHexC 'C' = [I,I,O,O]
parseHexC 'D' = [I,I,O,I]
parseHexC 'E' = [I,I,I,O]
parseHexC 'F' = [I,I,I,I]
parseHexC _ = []

toInt :: [Bit] -> Int
toInt = foldl (\n b -> (n*2) + toIntB b) 0
  where
    toIntB O = 0
    toIntB I = 1

data Packet = Const { val :: Int, ver :: Int } | Op { op :: Int, ver :: Int, packets :: [Packet] } deriving(Eq, Show)

type Parser = State [Bit]

sumVersions :: Packet -> Int
sumVersions (Const _ ver) = ver
sumVersions (Op _ ver sub) = ver + (sum $ map sumVersions sub)

pull :: Int -> Parser [Bit]
pull n = do
  bits <- gets $ take n
  modify $ drop n
  return bits

parsePacket :: Parser Packet
parsePacket = do
  (ver, tp) <- parseHeader
  case tp of
    4 -> parseConst ver
    _ -> parseOp ver tp

parseHeader :: Parser (Int, Int)
parseHeader = do
  ver <- parseVer
  tp <- parseType
  return (ver, tp)

parseVer :: Parser Int
parseVer = fmap toInt $ pull 3

parseType :: Parser Int
parseType = fmap toInt $ pull 3

parseConst :: Int -> Parser Packet
parseConst ver = do
  bits <- parseConstBytes
  let val = toInt bits
  return $ Const val ver

parseConstBytes :: Parser [Bit]
parseConstBytes = do
  bs <- pull 5
  case bs of
    O:bs' -> return bs'
    I:bs' -> fmap ((++) bs') parseConstBytes

parseOp :: Int -> Int -> Parser Packet
parseOp ver tp = do
  lengthType <- fmap head $ pull 1
  lengthBits <-
    case lengthType of
      O -> pull 15
      I -> pull 11
  let length = toInt lengthBits
  subPackets <-
    case lengthType of
      O -> parseSubPacketsByLength length
      I -> parseSubPacketsByCount length
  return $ Op tp ver subPackets

parseSubPacketsByLength :: Int -> Parser [Packet]
parseSubPacketsByLength 0 = return []
parseSubPacketsByLength n = do
  currentLength <- gets length
  subPacket <- parsePacket
  newLength <- gets length
  let subPacketLength = currentLength - newLength
  subPackets <- parseSubPacketsByLength (n - subPacketLength)
  return $ subPacket:subPackets

parseSubPacketsByCount :: Int -> Parser [Packet]
parseSubPacketsByCount 0 = return []
parseSubPacketsByCount n = do
  subPacket <- parsePacket
  subPackets <- parseSubPacketsByCount (n - 1)
  return $ subPacket:subPackets
