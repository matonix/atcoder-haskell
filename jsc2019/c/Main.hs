module Main where

m = 1000000007

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- getLine
  print $ solve n ss
  where
    solve n ss = 1
    