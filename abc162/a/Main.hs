module Main where
import qualified Data.ByteString.Char8         as BS

main :: IO ()
main = do
  s <- BS.getLine
  let b = BS.elem '7' s
  putStrLn $ if b then "Yes" else "No"
