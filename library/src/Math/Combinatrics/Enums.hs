module Math.Combinatrics.Enums where

import           Control.Monad

brutes :: [a] -> Int -> [[a]]
brutes x n = replicateM n x

perms :: [a] -> Int -> [[a]]
perms [] _    = [[]]
perms ns size = perm' size (length ns - 1) [(ns, [])]
 where
  perm' 0 _ xs = [ a | (_, a) <- xs ]
  perm' c n xs = perm' (c - 1) (n - 1) $ concatMap (f n) xs
  f n (xs, ys) =
    [ (as ++ bs, ys ++ [b]) | x <- [0 .. n], let (as, b : bs) = splitAt x xs ]

combs :: [a] -> Int -> [[a]]
combs [] _    = [[]]
combs ns size = comb' size [(ns, [])]
 where
  comb' 0 xs = [ a | (_, a) <- xs ]
  comb' c xs = comb' (c - 1) $ concatMap comb'' xs
  comb'' (x : xs, ys) = (xs, ys ++ [x]) : comb'' (xs, ys)
  comb'' _            = []
