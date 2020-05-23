{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import           Data.Ratio

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap

main :: IO ()
main = do
  n <- readLn @ Int
  abs <- V.replicateM n $ (\[x, y] -> (x, y)) . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let abs' = V.map f abs
  let h = HashMap.fromListWith (+) [ (x, 1) | x <- V.toList abs' ]
  print h

f :: (Int, Int) -> Ratio Int
f (a, b) = abs a % abs b