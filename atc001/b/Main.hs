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
import           Control.Monad.Primitive

main :: IO ()
main = do
  [n, q] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  qs <- V.replicateM q $ VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  uf <- initUf n
  go qs uf 0 q
 where
  go qs uf i q =
    if i == q 
    then return ()
    else do
      let query = qs V.! i
      case query VU.! 0 of
        0 -> do
          uf' <- uniteUf uf (query VU.! 1) (query VU.! 2)
          go qs uf' (i+1) q
        1 -> do
          (uf', isConnect) <- sameUf uf (query VU.! 1) (query VU.! 2)
          putYN isConnect
          go qs uf' (i+1) q

putYN :: Bool -> IO ()
putYN True  = putStrLn "Yes"
putYN False = putStrLn "No"

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
