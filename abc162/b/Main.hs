module Main where

import           Data.Int

main :: IO ()
main = do
  x <- readLn :: IO Int64
  print $ sum $ filter f [1..x]
  where
    f x = x `mod` 3 /= 0 && x `mod` 5 /= 0