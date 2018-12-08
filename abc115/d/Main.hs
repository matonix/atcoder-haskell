module Main where

import Control.Monad
import Data.List
import Debug.Trace
  
main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine :: IO [Integer] -- (< 50, exp)
  print $ pat n x
  where
    pat 0 x = 1
    pat n x
      | x < 2 = 0
      | x < len (n-1) + 2 = pat (n-1) (x-1)
      | x < len (n-1) + 3 = 1 + pat (n-1) (x-1)
      | x < len n = 1 + pat (n-1) (x-1) + pat (n-1) (x-len(n-1)-2)
      | otherwise = 1 + pat (n-1) (x-1) * 2
    len 0 = 1
    len n = len (n - 1) * 2 + 3