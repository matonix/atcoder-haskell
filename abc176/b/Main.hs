module Main where

main :: IO ()
main = do
  x <- readLn :: IO Integer
  putYN $ x `mod` 9 == 0

putYN :: Bool -> IO ()
putYN True  = putStrLn "Yes"
putYN False = putStrLn "No"