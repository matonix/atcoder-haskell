module Main where

import Control.Applicative

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ if even $ a * b then "Even" else "Odd"