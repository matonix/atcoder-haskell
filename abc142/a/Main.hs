module Main where

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  Just (i, _) <- BS.readInt <$> BS.getLine
  print $ 1 - fromIntegral (i `div` 2) / fromIntegral i