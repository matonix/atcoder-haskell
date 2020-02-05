module Main where

import qualified Data.ByteString.Char8         as BS
import           Data.List                      ( unfoldr )
import           Data.Char                      ( isSpace )
import           Debug.Trace
import qualified Data.Vector.Algorithms.Intro  as VA
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

main :: IO ()
main = do
  [n, k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  hs     <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  sorted <- do
    hs' <- VU.unsafeThaw hs
    VA.sort hs'
    VU.unsafeFreeze hs'
  print $ VU.sum $ VU.drop k $ VU.reverse sorted
