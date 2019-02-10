module Main where

main :: IO ()
main = do
  [n, k] <- map (read::String -> Int) . words <$> getLine
  putStrLn $ if k <= (n + 1) `div` 2 then "YES" else "NO"
