{-# LANGUAGE PatternSynonyms #-}
module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import           Data.Maybe
import Control.Monad as M
import Prelude as P
import qualified Data.List                     as L
-- http://hackage.haskell.org/package/massiv-0.4.5.0
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector
import Data.Sequence as Seq
import Debug.Trace

main :: IO ()
main = do
  [h, w] <- L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  [ch, cw] <- L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  [dh, dw] <- L.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  arr <- fromLists' Seq <$> M.replicateM h P.getLine :: IO (Array U Ix2 Char)
  marr <- A.thaw arr
  let start = Ix2 (ch-1) (cw-1)
  let goal = Ix2 (dh-1) (dw-1)
  let q = Seq.singleton (start, 0)
  ans <- bfs marr q goal
  arr' <- A.freeze Seq marr
  -- print arr'
  print ans

bfs :: MArray RealWorld U Ix2 Char -> Seq (Ix2, Int) -> Ix2 -> IO Int
bfs marr Empty goal = return (-1)
bfs marr ((cPos,c) :<| q) goal
  | cPos == goal = return c
  | otherwise = do
      -- print (cPos, c)
      -- check visited
      b <- isPath marr cPos
      if b
      then do
        A.write_ marr cPos '#'
        q' <- makeq marr q cPos c
        bfs marr q' goal
      else do
        bfs marr q goal

isPath :: MArray RealWorld U Ix2 Char -> Ix2 -> IO Bool
isPath m p = do
  x <- A.read m p
  case x of
    Nothing -> return False
    Just y -> return $ y == '.'

makeq :: MArray RealWorld U Ix2 Char -> Seq (Ix2, Int) -> Ix2 -> Int -> IO (Seq (Ix2, Int))
makeq m q pos cost = do
  walk' <- M.filterM (isPath m) $ P.map (pos+) walk
  warp' <- M.filterM (isPath m) $ P.map (pos+) warp
  return $ Seq.fromList (P.zip walk' $ repeat cost) >< q >< Seq.fromList (P.zip warp' $ repeat (cost + 1))

walk = [  1 :.  0
       , -1 :.  0
       ,  0 :.  1
       ,  0 :. -1
       ]

warp = [ Ix2 i j | i <- [-2..2], j <- [-2..2], abs i + abs j >= 2 ]