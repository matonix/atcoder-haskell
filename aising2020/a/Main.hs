module Main where

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )

main :: IO ()
main = do
  [l, r, d] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ length [x | x <- [l..r], x `mod` d == 0]
