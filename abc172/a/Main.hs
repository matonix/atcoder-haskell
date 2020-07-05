module Main where

main :: IO ()
main = do
  a <- readLn::IO Int
  print$  a + a^2 + a^3
