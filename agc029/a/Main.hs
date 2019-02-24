module Main where

main :: IO ()
main = do
  s <- getLine
  let w = numOfWs s
  let ws = take w [1..] :: [Int]
  let ws' = posOfWs s :: [Int]
  print $ sum $ zipWith (-) ws' ws
  where
    numOfWs = length . filter (=='W')
    posOfWs = map fst . filter ((=='W') . snd) . zip [1..]