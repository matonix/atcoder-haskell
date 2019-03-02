module Main where

import Control.Monad
import Control.Applicative

main :: IO ()
main = do
  [a, b, q] <- map read . words <$> getLine :: IO [Int]
  ss <- replicateM a $ read <$> getLine :: IO [Int64]
  ts <- replicateM b $ read <$> getLine :: IO [Int64]
  xs <- replicateM q $ read <$> getLine :: IO [Int64]
  mapM_ print $ solve ss ts xs

solve ss ts xs = map ans xs
  where
    ans x = binSearch x intervals
    intervals :: (Int64, Int64) 
    additionalCost :: IntMap 