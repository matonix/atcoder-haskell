module Main where

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Algorithms.Tim    as VAT
import           Data.Heap                      ( Heap )
import qualified Data.Heap                     as Heap
import           Data.Ord

main :: IO ()
main = do
  n   <- readLn :: IO Int
  as  <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as' <- do
    x <- VU.thaw as
    VAT.sortBy (comparing Down) x
    VU.freeze x
  let s   = Heap.singleton $ Down $ VU.head as'
  let ans = VU.foldl' go (s, 0) $ VU.tail as'
  print $ snd ans
 where
  go :: (Heap (Down Int), Int) -> Int -> (Heap (Down Int), Int)
  go (s, c) a =
    let Down v = Heap.minimum s -- maximum
        s'     = Heap.insert (Down a) $ Heap.insert (Down a) $ Heap.deleteMin s -- deleteMax
    in  (s', c + v)

