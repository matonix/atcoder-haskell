module Data.UnionFind.VectorSpec where

import Data.Vector.Unboxed.Mutable as VUM
import Data.UnionFind.Vector
import Test.Hspec

main :: IO ()
main = hspec spec

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "initUf" $ do
    it "initial parent of 3rd item should be 2" $ do
      pick3rd `shouldReturn` 2
  describe "uniteUf" $ do
    it "size of united 3 item should be 3" $ do
      unite3 `shouldReturn` 3

pick3rd :: IO Int
pick3rd = do
  uf <- initUf 3
  VUM.read (parent uf) 2

unite3 :: IO Int
unite3 = do
  uf <- initUf 3
  uf' <- uniteUf uf 0 1
  uf'' <- uniteUf uf' 0 2
  sizeUf uf'' 2
