module Main where

main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ if n `mod` 1000 == 0 then 0 else 1000 - n `mod` 1000
