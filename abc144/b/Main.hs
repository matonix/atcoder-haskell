module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Char

main :: IO ()
main = do
  [n] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  putStrLn $ if elem n list then "Yes" else "No"
  where
    list = (*) <$> [1..9] <*> [1..9]
