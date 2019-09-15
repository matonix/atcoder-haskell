module Main where

import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Integer
  print $ n * (n - 1) `div` 2
  
  -- forM_ [1..10] $ \n -> do
  --   let xs = [1..n]
  --   print $ maximum $ map (sum . modOrig xs) $ perm2 xs n

-- modOrig x y = zipWith mod y x


-- perm2 :: Eq a => [a] -> Int -> [[a]]
-- perm2 _ 0 = [[]]
-- perm2 xs n = [x:ys | x <- xs, ys <- perm2 xs (n-1)]