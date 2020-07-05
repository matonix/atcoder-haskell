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
import Data.Vector.Algorithms.Search
import           Control.Monad                  ( forM )

main :: IO ()
main = do
  [n, m, k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  bs <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as' <- VU.thaw $ VU.scanl1' (+) as
  bs' <- VU.thaw $ VU.scanl1' (+) bs
  ansA <- solve as' bs' k
  ansB <- solve bs' as' k
  print $ max ansA ansB

solve as' bs' k = do
  -- get index of maximum cost of as
  i <- binarySearchR as' k
  anss <- if i == 0 
    then do
      j <- binarySearchR bs' k
      return $ VU.singleton j
    else
      VU.forM (VU.enumFromN 0 i) $ \i' -> do
        accumA <- VUM.read as' i'
        j <- binarySearchR bs' (k-accumA)
        return (i' + j + 1)
  return $ VU.maximum anss
