module Main where

main :: IO ()
main = do
  n <- readLn
  if n == 1
    then putStrLn "Hello World"
    else do
      a <- readLn
      b <- readLn
      print $ a + b
  