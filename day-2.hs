#!/usr/bin/env stack
-- stack --resolver=lts-18.18 script

main :: IO ()
main = do
  directions <- getContents
  let (horizontal, depth) = navigateSub $ map readMovement $ lines directions
  putStrLn $ "Horizontal: " ++ (show horizontal)
  putStrLn $ "Depth: " ++ (show depth)
  putStrLn $ "Total: " ++ (show $ horizontal * depth)

readMovement :: String -> Movement
readMovement s = (dir, read distance)
  where (dir:distance:[]) = words s

navigateSub :: [Movement] -> Location
navigateSub = foldl move (0, 0)

type Location = (Int, Int)
type Movement = (String, Int)

move :: Location -> Movement -> Location
move (x, d) ("forward", x') = (x + x', d)
move (x, d) ("up", d') = (x, d - d')
move (x, d) ("down", d') = (x, d + d')
