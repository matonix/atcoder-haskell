module Main where

import Debug.Trace

main :: IO ()
main = do
  s <- getLine
  putStrLn $ ans $ all p $ zip s $ map odd [1..]
  where
    ans True = "Yes"
    ans False = "No"
    p ('L', True) = False
    p (_, True) = True
    p ('R', False) = False
    p (_, False) = True
