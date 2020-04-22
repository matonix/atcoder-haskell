module Main where

import qualified Data.ByteString.Char8 as BS
import           Data.List                      ( unfoldr )
import           Data.Char                      ( isSpace )

main :: IO ()
main = do
  [h, a] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ (h + a - 1) `div` a