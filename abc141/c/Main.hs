module Main where

import           Control.Monad
import           Data.List
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap

main :: IO ()
main = do
  [n, k, q] <- map (read :: String -> Int) . words <$> getLine
  as        <- replicateM q $ (read :: String -> Int) <$> getLine
  mapM_ (putStrLn . ans . (> 0)) $ solve n k q as
 where
  ans True  = "Yes"
  ans False = "No"
  cnt :: [Int] -> [(Int, Int)]
  cnt as' = IntMap.toDescList $ IntMap.fromListWith (+) (zip as' [1, 1 ..])
  solve n k q as = go [] n (cnt as)
   where
    go acc 0      _                  = acc
    go acc member []                 = go (k - q : acc) (member - 1) []
    go acc member ((a, c) : answers) = if member == a
      then go (k - q + c : acc) (member - 1) answers
      else go (k - q : acc) (member - 1) ((a, c) : answers)
