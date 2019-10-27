module Main where

import qualified Data.ByteString.Char8         as BS
import           Data.List
import           Data.Char

main :: IO ()
main = do
  [a, b, x] <- map fromIntegral . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let s = x / a :: Double
  print $ if s >= a * b / 2
    then let c = (2 * b) - (2 * s / a) in 90 - atan (a / c) / pi*180
    else let c = (2 * s) / b in 90 - atan (c / b) / pi*180
