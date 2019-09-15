module Main where

main :: IO ()
main = do
  [a, b] <- map (read::String -> Int) . words <$> getLine
  print $ (b - 1) `div` (a - 1) + if (b - 1) `mod` (a - 1) == 0 then 0 else 1