module Main where

import Data.List

main :: IO ()
main = do
  a <- (read <$> getLine) :: IO Int
  putStrLn $ unwords $ "Christmas" : replicate (25 - a) "Eve" 
