module Main where

import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- replicateM n readLn :: IO [Int]
  let pm = maximum ps
  print $ sum ps - (pm `div` 2)
