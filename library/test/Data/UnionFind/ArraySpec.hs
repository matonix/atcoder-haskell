module Data.UnionFind.ArraySpec where

import Data.Array.IO
import Data.UnionFind.Array
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "initUf" $ do
    it "initial parent of 3rd item should be 3" $ do
      pick3rd `shouldReturn` 3
  describe "uniteUf" $ do
    it "size of united 3 item should be 3" $ do
      unite3 `shouldReturn` 3

pick3rd :: IO Int
pick3rd = do
  uf <- initUf 3
  readArray (parent uf) 3

unite3 :: IO Int
unite3 = do
  uf <- initUf 3
  uf' <- uniteUf uf 1 2
  uf'' <- uniteUf uf' 1 3
  sizeUf uf'' 3
