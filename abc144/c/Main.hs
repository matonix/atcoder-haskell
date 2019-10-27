module Main where

import qualified Data.ByteString.Char8         as BS
import           Data.List
import           Data.Char
import           Control.Arrow
import qualified Data.IntSet                   as IntSet
import qualified Data.Vector.Unboxed           as VU

main :: IO ()
main = do
  [n] <- unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
  print $ solve n
 where
  solve n = minimum
    $ map (\x -> if mod n x == 0 then x - 1 + div n x - 1 else n) [1 .. sqn]
    where sqn = floor $ sqrt $ fromIntegral n
