module Main where

import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Unboxed           as VU
import           Control.Monad.Primitive
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )


main :: IO ()
main = do
  [n, m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  pairs <- VU.replicateM m $ (\v -> (v VU.! 0 - 1, v VU.! 1 - 1)) . VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  uf <- initUf n
  uf' <- VU.foldM (\uf (a, b) -> uniteUf uf a b) uf pairs
  sizes <- mapM (sizeUf uf') [0..n-1]
  print $ maximum sizes

data UnionFind s = UnionFind
  { parent :: VUM.MVector s Int
  , size :: VUM.MVector s Int
  }

-- Vector internally uses 0-indexed

initUf :: PrimMonad m => Int -> m (UnionFind (PrimState m))
initUf n = UnionFind <$> VU.thaw (VU.enumFromN 0 n) <*> VUM.replicate n 1

rootUf
  :: PrimMonad m
  => UnionFind (PrimState m)
  -> Int
  -> m (UnionFind (PrimState m), Int)
rootUf uf x = do
  px <- VUM.read (parent uf) x
  if px == x
    then return (uf, x)
    else do
      (UnionFind parent' size', rx) <- rootUf uf px
      VUM.write parent' x rx
      return (UnionFind parent' size', rx)

sameUf
  :: PrimMonad m
  => UnionFind (PrimState m)
  -> Int
  -> Int
  -> m (UnionFind (PrimState m), Bool)
sameUf uf x y = do
  (uf' , rx) <- rootUf uf x
  (uf'', ry) <- rootUf uf' y
  return (uf'', rx == ry)

uniteUf
  :: PrimMonad m
  => UnionFind (PrimState m)
  -> Int
  -> Int
  -> m (UnionFind (PrimState m))
uniteUf uf x y = do
  (uf'                           , x') <- rootUf uf x
  (uf''@(UnionFind parent' size'), y') <- rootUf uf' y
  if x' == y'
    then return uf''
    else do
      sizeX <- VUM.read size' x'
      sizeY <- VUM.read size' y'
      if sizeX < sizeY
        then do
          VUM.write parent' x' y'
          VUM.write size' y' (sizeX + sizeY)
          return $ UnionFind parent' size'
        else do
          VUM.write parent' y' x'
          VUM.write size' x' (sizeX + sizeY)
          return $ UnionFind parent' size'

sizeUf :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeUf uf x = do
  (UnionFind _ size', x') <- rootUf uf x
  VUM.read size' x'
