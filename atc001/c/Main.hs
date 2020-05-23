{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )

main :: IO ()
main = do
  n <- readLn @ Int
  abs <- V.replicateM n $ VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  putStrLn "hoge"

-- http://wwwa.pikara.ne.jp/okojisan/stockham/cooley-tukey.html
f n q x = do
  let m = n `div` 2
  let theta0 = 2 * pi / n :: Double
  (f0, f1) <- forM [0..nd2] $ \i -> do
    f0 <- VUM.new (nd2)
    f1 <- VUM.new (nd2)
    x0 <- VUM.read (2*i+0)
    f0 <- VUM.write f x0
    x1 <- VUM.read (2*i+1)
    f1 <- VUM.write f x1
    
  f0 <- dft f0 nd2
  VUM.read f
