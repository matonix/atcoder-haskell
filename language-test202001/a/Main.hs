module Main where

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )

main :: IO ()
main = do
  a      <- readLn
  [b, c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  s      <- BS.getLine
  BSL.putStrLn
    $  BSB.toLazyByteString
    $  BSB.intDec (a + b + c)
    <> BSB.char7 ' '
    <> BSB.byteString s
