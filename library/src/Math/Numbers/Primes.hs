module Math.Numbers.Primes where

import           Control.Arrow
import           Data.List

-- http://qiita.com/myuon_myon/items/ad006568bd187223f494#2-4
primes :: Integral a => [a]
primes = 2:3:[x|i<-[1..], j<-[-1,1], let x = 6*i+j, isPrime x] where
  isPrime n = null [i|i<-takeWhile (\x -> x*x <= n) primes, rem n i == 0]

-- | prime factors
-- returns [(PrimeFactor, Exponent)]
-- >>> primeFactor 9
-- [(3,2)]
primeFactor :: Integral a => a -> [(a, Int)]
primeFactor x = map (head &&& length) . group $ go x divisor
  where
    go 1 [] = []
    go y [] = [y]
    go y ps'@(p : ps) = case quotRem y p of
      (q, 0) -> p : go q ps'
      _ -> go y ps
    ub = floor . (sqrt :: Double -> Double) $ fromIntegral x
    divisor = takeWhile (<= ub) primes