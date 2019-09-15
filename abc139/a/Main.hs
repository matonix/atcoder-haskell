module Main where

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  print $ length $ filter (uncurry (==)) $ zip s t