module Math.Combinatrics.Counts where

import           Data.Array

-- nCk : binomial coefficents
binos :: Integral a => a -> a -> a
binos n k = bino' n (if k * 2 <= n then k else n - k) where
  bino' _  0  = 1
  bino' 0  _  = 0
  bino' n' k' = binos (n' - 1) (k' - 1) * n' `div` k'

-- nHk : multiset coefficients
multisetCoefficient :: Integral a => a -> a -> a
multisetCoefficient n k = binos (n + k - 1) k

-- Catalan numbers
catalans :: Integral a => a -> a
catalans n = binos (2 * n) n `div` (n + 1)

-- Stirling number of the second kind
stirling2 :: (Ix a, Num a) => a -> a -> a
stirling2 n k = iter (n, k) where
  memo = listArray ((0, 0), (n, k)) $ map iter $ range ((0, 0), (n, k))
  iter (n', k') | n' == k'  = 1
                | k' == 0   = 0
                | otherwise = memo ! (n' - 1, k' - 1) + k' * memo ! (n' - 1, k')
