module Data.UnionFind.Array where

import Data.Array.IO

data UnionFind = UnionFind
  { parent :: IOUArray Int Int
  , size :: IOUArray Int Int
  }

initUf :: Int -> IO UnionFind
initUf n = UnionFind
  <$> newListArray (1, n) [1..n]
  <*> newArray (1, n) 1

rootUf :: UnionFind -> Int -> IO (UnionFind, Int)
rootUf uf x = do
  px <- readArray (parent uf) x
  if px == x 
  then return (uf, x)
  else do
    (UnionFind parent' size', rx) <- rootUf uf px
    writeArray parent' x rx
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
    sizeX <- readArray size' x'
    sizeY <- readArray size' y'
    if sizeX < sizeY
    then do
      writeArray parent' x' y'
      writeArray size' y' (sizeX + sizeY)
      return $ UnionFind parent' size'
    else do
      writeArray parent' y' x'
      writeArray size' x' (sizeX + sizeY)
      return $ UnionFind parent' size'
    
sizeUf :: UnionFind -> Int -> IO Int
sizeUf uf x = do
  (UnionFind _ size', x') <- rootUf uf x
  readArray size' x'
