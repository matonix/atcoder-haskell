-- cf. Haskellで戦う競技プログラミング p.61
-- Sieve of Eratosthenes O(n log log n)
module Math.Numbers.Primes.Vector where

import           Control.Monad
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

type PrimeTable = VU.Vector Bool

primeTableToList :: PrimeTable -> [Int]
primeTableToList t = [ i | i <- [0 .. VU.length t - 1], t VU.! i ]

isInPrimeTable :: PrimeTable -> Int -> Bool
isInPrimeTable t n | 0 <= n && n < VU.length t = t VU.! n
                   | otherwise                 = error "out of range"

sieve :: Int -> PrimeTable
sieve n = VU.create $ do
  vec <- VUM.replicate (n + 1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2 .. n] $ \i -> do
    b <- VUM.read vec i
    when b $ forM_ [2 * i, 3 * i .. n] $ \j -> VUM.write vec j False
  return vec

-- Usage

sieveSample :: IO ()
sieveSample = do
  let t = sieve 100
  print (primeTableToList t)