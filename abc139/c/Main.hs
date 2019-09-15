module Main where

-- import Debug.Trace

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- map (read::String -> Int) . words <$> getLine
  print $ go hs 0 0
  where
    go [x] l mx = (max mx l)
    go (x:y:xs) l mx = if x >= y
      then go (y:xs) (l+1) mx
      else go (y:xs) 0 (max mx l)
