{-# LANGUAGE TypeApplications #-}
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

main :: IO ()
main = do
  t <- readLn @ Int
  qs <- V.replicateM t $ (\[n, a, b, c, d] -> (n, a, b, c, d)) . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  V.mapM_ (print . solve) qs

solve :: (Int, Int, Int, Int, Int) -> Int
solve (n, a, b, c, d) = undefined