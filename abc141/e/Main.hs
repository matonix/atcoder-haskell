module Main where

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  s <- getLine
  print $ solve n s
  where
    solve n s = sufs
