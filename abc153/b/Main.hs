module Main where

import qualified Data.ByteString.Char8         as BS
import           Data.List                      ( unfoldr )
import           Data.Char                      ( isSpace )
import           Debug.Trace

main :: IO ()
main = do
  [h, n] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as     <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  putStrLn $ yn $ traceShowId $ h <= sum as

yn True  = "Yes"
yn False = "No"
