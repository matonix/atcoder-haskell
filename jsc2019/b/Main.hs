module Main where

m = 1000000007

main :: IO ()
main = do
  [n, k] <- map (read :: String -> Integer) . words <$> getLine
  as <- map (read :: String -> Int) . words <$> getLine
  print $ solve n k as
  where
    solve n k as =
      let
        inner = f as
        inter_all = f (as ++ as)
        inter = inter_all - (2 * inner)
      in ((inter * (k * (k - 1) `div` 2)) + k * inner) `mod` m

f :: [Int] -> Integer
f [] = 0 
f (a:as) = fromIntegral (length (filter (a >) as)) + f as