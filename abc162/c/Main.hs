module Main where

main :: IO ()
main = do
  k <- readLn :: IO Int
  let xs = [1 .. k]
  print $ sum [ a `gcd` b `gcd` c | a <- xs, b <- xs, c <- xs ]
