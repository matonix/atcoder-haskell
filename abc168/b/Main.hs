{-# LANGUAGE TypeApplications #-}
module Main where

main :: IO ()
main = do
  k <- readLn @ Int
  s <- getLine
  putStrLn $ if length s > k then take k s ++ "..." else s

  