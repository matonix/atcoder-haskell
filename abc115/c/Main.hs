module Main where
  
import Control.Monad
import Data.List
import Debug.Trace

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine :: IO [Int] -- 10^5
  xs <- replicateM n readLn :: IO [Int] -- 10^9
  let ss = sort xs
  let ls = ss
  let hs = drop (k-1) ss
  print $ go ls hs (maxBound::Int)
  where
    go _ [] cost = cost
    go (l:ls) (h:hs) cost =  
      go ls hs (cost `min` (h - l))
