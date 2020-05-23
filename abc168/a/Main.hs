module Main where

main :: IO ()
main = do
  x <- getLine 
  putStrLn $ case last x of
    '3' -> "bon"
    '0' -> "pon"
    '1' -> "pon"
    '6' -> "pon"
    '8' -> "pon"
    _ -> "hon"
