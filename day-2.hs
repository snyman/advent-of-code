#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script

main :: IO ()
main = do
  directions <- getContents
  let (horizontal, depth, aim) = navigateSub $ map readMovement $ lines directions
  putStrLn $ "Horizontal: " ++ (show horizontal)
  putStrLn $ "Depth: " ++ (show depth)
  putStrLn $ "Aim: " ++ (show aim)
  putStrLn $ "Total: " ++ (show $ horizontal * depth)

readMovement :: String -> Movement
readMovement s = (dir, read distance)
  where (dir:distance:[]) = words s

navigateSub :: [Movement] -> Location
navigateSub = foldl move (0, 0, 0)

type Location = (Int, Int, Int) -- horizontal, depth, aim
type Movement = (String, Int)

move :: Location -> Movement -> Location
move (x, d, a) ("forward", distance) = (x + distance, d + a * distance, a)
move (x, d, a) ("up", distance) = (x, d, a - distance)
move (x, d, a) ("down", distance) = (x, d, a + distance)
