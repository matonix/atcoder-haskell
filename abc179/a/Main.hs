module Main where

main :: IO ()
main = do
  x <- getLine
  putStrLn $ if last x == 's' then x ++ "es" else x ++ "s"