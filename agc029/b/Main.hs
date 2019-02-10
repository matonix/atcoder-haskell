module Main where

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as Map
import Control.Monad

pows = Set.fromList $ take 33 $ iterate (*2) 1

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Int]
  let m = Map.fromListWith (+) $ zip as (repeat 1)
  print $ go m 0
  where
    go :: IntMap Int -> Int -> Int
    go m n 
      | Map.null m = n
      | otherwise = let 
        (maxKey, cnt) = Map.findMax m 
        Just nextPow = Set.lookupGT maxKey pows
        m' = delete maxKey m
        key = nextPow - maxKey
        in if Map.member key m'
          then go (delete key m') (n+1)
          else go m' n

delete :: Int -> IntMap Int -> IntMap Int
delete k m = case Map.lookup k m of
  Nothing -> error "delete"
  Just 1 -> Map.delete k m
  Just n -> Map.update (Just . pred) k m