module Main where

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import           Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  cases <- V.replicateM t readCase
  V.mapM_ (print . solve) cases

readCase = do
  n <- readLn :: IO Int
  VU.replicateM n $ toTriple . VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

toTriple :: VU.Unbox a => VU.Vector a -> (a, a, a)
toTriple v = (v VU.! 0, v VU.! 1, v VU.! 2)

solve :: VU.Vector (Int, Int, Int) -> Int
solve = undefined 