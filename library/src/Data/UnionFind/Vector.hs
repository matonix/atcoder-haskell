module Data.UnionFind.Vector where

import Data.Vector.Unboxed.Mutable as VUM
import Data.Vector.Unboxed as VU

data UnionFind = UnionFind
  { parent :: VUM.IOVector Int
  , size :: VUM.IOVector Int
  }

-- Vector internally uses 0-indexed

initUf :: Int -> IO UnionFind
initUf n = UnionFind
  <$> VU.thaw (VU.fromList [0..n-1])
  <*> VUM.replicate n 1

rootUf :: UnionFind -> Int -> IO (UnionFind, Int)
rootUf uf x = do
  px <- VUM.read (parent uf) x
  if px == x 
  then return (uf, x)
  else do
    (UnionFind parent' size', rx) <- rootUf uf px
    VUM.write parent' x rx
    return (UnionFind parent' size', rx)

sameUf :: UnionFind -> Int -> Int -> IO (UnionFind, Bool)
sameUf uf x y = do
  (uf', rx) <- rootUf uf x
  (uf'', ry) <- rootUf uf' y
  return (uf'', rx == ry)

uniteUf :: UnionFind -> Int -> Int -> IO UnionFind
uniteUf uf x y = do
  (uf', x') <- rootUf uf x
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
    
sizeUf :: UnionFind -> Int -> IO Int
sizeUf uf x = do
  (UnionFind _ size', x') <- rootUf uf x
  VUM.read size' x'
