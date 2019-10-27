module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Char

main :: IO ()
main = do
  [a, b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ if a < 10 && b < 10
    then a * b
    else -1