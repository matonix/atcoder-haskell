module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Char
import qualified Data.Map.Strict as M
import Control.Arrow
-- import Debug.Trace

main :: IO ()
main = do
  [a, b] <- unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
  print . succ . pf $ gcd a b

-- http://qiita.com/myuon_myon/items/ad006568bd187223f494#2-4
primes = 2:3:[x|i<-[1..], j<-[-1,1], let x = 6*i+j, isPrime x] where
  isPrime n = null [i|i<-takeWhile (\x -> x*x <= n) primes, rem n i == 0]

-- | prime factors with 0-factor 
-- returns [(PrimeFactor, Exponent)]
-- >>> primeFactor 9
-- [(2,0),(3,2)]
primeFactor x = go x primes
  where       
    ub = floor . sqrt $ fromIntegral x
    go x (p:ps) 
      | p <= ub = let (x', e) = gogo x p 0 
        in (p, e) : go x' ps
      | x == 1 = []
      | otherwise = [(x, 1)]
      where
        gogo x p e = case quotRem x p of
          (q, 0) -> gogo q p (e + 1)
          (q, _) -> (x, e)

-- | prime factors without 0-factor 
-- returns [(PrimeFactor, Exponent)]
-- >>> primeFactor' 9
-- [(3,2)]
primeFactor' x = map (head &&& length) . group $ go x divisor
  where
    go 1 [] = []
    go y [] = [y]
    go y ps'@(p : ps) = case quotRem y p of
      (q, 0) -> p : go q ps'
      (q, _) -> go y ps
    ub = floor . sqrt $ fromIntegral x
    divisor = takeWhile (<= ub) primes

pf x = length . group $ go x divisor
  where
    go 1 [] = []
    go y [] = [y]
    go y ps'@(p : ps) = case quotRem y p of
      (q, 0) -> p : go q ps'
      (q, _) -> go y ps
    ub = floor . sqrt $ fromIntegral x
    divisor = takeWhile (<= ub) primes
