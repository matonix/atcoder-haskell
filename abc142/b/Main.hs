module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Char

main :: IO ()
main = do
  [n, k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  hs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ length $ filter (>=k) hs