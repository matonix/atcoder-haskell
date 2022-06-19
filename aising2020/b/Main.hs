module Main where

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ VU.length $ VU.ifilter (\i x -> even i && odd x) as
