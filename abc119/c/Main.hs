module Main where

import Data.List (permutations)
import Data.Maybe (mapMaybe)
import Control.Monad
import qualified Data.Set as S
import qualified Data.IntSet as I
import Debug.Trace

data D = D I.IntSet Int Int -- indeces, length, additinal cost
  deriving (Show, Eq)

instance Ord D where
  D _ l _ <= D _ l' _ = l <= l'

main :: IO ()
main = do
  [n, a, b, c] <- map read . words <$> getLine :: IO [Int]
  ls <- replicateM n $ read <$> getLine :: IO [Int]
  print . minimum . mapMaybe (go (initSet n ls) 0) $ permutations [a, b, c]
    where
      initSet n ls = S.fromList [ (\(is, ls') -> 
        D (I.fromList is) (sum ls') ((length ls' - 1) * 10)) $ unzip ts 
        | ts <- concatMap (comb5 $ zip [1..] ls) [1..(n - 3 + 1)] ]
      go _ currentCost [] = Just currentCost
      go set currentCost (obj:objs) 
        | S.null set = Nothing
        | otherwise = 
          let 
            (is, cost) = getMinimum set obj 
            newSet = filterD is set
            newCost = currentCost + cost
          in go newSet newCost objs 
          where
            getMinimum :: S.Set D -> Int -> (I.IntSet, Int)
            getMinimum set obj = 
              let (D is l c) = S.findMin $ S.map (f obj) set -- when #bamboo is small, bamboos may dry up (and get RE)
              in (is, l)
              where
                f obj (D is l c) = D is ((abs (l - obj)) + c) c
            filterD :: I.IntSet -> S.Set D -> S.Set D
            filterD is set = 
              S.filter (\(D is' _ _) -> I.null $ is `I.intersection` is') set

comb5 :: [a] -> Int -> [[a]]
comb5 [] _ = [[]]
comb5 ns size = comb' size [(ns, [])]
    where
      comb' 0 xs = [a | (_, a) <- xs]
      comb' c xs = comb' (c - 1) $ concatMap comb'' xs
      comb'' (x : xs, ys) = (xs, ys ++ [x]) : comb'' (xs, ys)
      comb'' _ = []