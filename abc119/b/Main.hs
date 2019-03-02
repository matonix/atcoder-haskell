module Main where

import Control.Monad
import Text.Printf

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  xus <- replicateM n $ read' . words <$> getLine
  printf "%.6f" $ sum xus

rate :: Double
rate = 380000

read' :: [String] -> Double
read' [x, u] = read x * if u == "BTC" then rate else 1