module Main where

import Control.Applicative

main :: IO ()
main = do
  a <- readLn
  [b, c] <- map read . words <$> getLine
  s <- getLine
  putStrLn $ show (a + b + c) ++ " " ++ s