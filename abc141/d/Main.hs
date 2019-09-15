module Main where

import           Data.IntSet             ( IntSet )
import qualified Data.IntSet            as IntSet

main :: IO ()
main = do
  [n, m] <- map (read :: String -> Int) . words <$> getLine
  as     <- map (read :: String -> Int) . words <$> getLine
  print $ solve n m (IntSet.toDescList $ IntSet.fromList as)
 where
  base = floor . logBase 2 . fromIntegral
  solve n m as = if sum (map base as) <= m 
    then 0 
    else go m as
    where
      go 0 xs = sum xs
      go r (x:xs) = let bx = base x in
        if bx <= m
        then go (r - bx) xs
        else go 0 (x `div` (2^bx) : xs)
