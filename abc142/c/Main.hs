module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Char
import Data.Function

main :: IO ()
main = do
  [n] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  putStrLn . unwords . map show $ map fst . sortBy (compare `on` snd) $ zip [1..] as
