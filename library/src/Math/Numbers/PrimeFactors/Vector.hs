-- cf. Haskellで戦う競技プログラミング p.61
-- Sieve of Eratosthenes O(n log log n)
module Math.Numbers.PrimeFactors.Vector where

import           Control.Monad
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Control.Arrow
import           Data.List
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet

-- Prime factor

type LDTable = VU.Vector Int

-- | prime factors
-- returns [(PrimeFactor, Exponent)]
-- >>> primeFactor (sieveLD 9) 9
-- [(3,2)]
primeFactor :: LDTable -> Int -> [(Int, Int)]
primeFactor t x = map (head &&& length) . group $ primeFactorRaw t x 

-- | prime factors without counts
-- returns [PrimeFactor]
-- >>> primeFactor' (sieveLD 12) 12
-- [2,3]
primeFactor' :: LDTable -> Int -> [Int]
primeFactor' t x = map head . group $ primeFactorRaw t x 

primeFactorS :: LDTable -> Int -> IntSet
primeFactorS t x = IntSet.fromAscList $ primeFactorRaw t x 

primeFactorRaw :: LDTable -> Int -> [Int]
primeFactorRaw _ 1 = []
primeFactorRaw t x = let f = t VU.! x in f : primeFactorRaw t (x `div` f)

-- Least Divisor
-- LD(4) = 2
-- LD(6) = 2
-- LD(35) = 5

sieveLD :: Int -> LDTable
sieveLD n = VU.create $ do
  vec <- VU.thaw $ VU.enumFromN 0 (n + 1)
  VUM.write vec 0 1
  forM_ [2 .. n] $ \i -> do
    d <- VUM.read vec i
    when (i == d) $ forM_ [2 * i, 3 * i .. n] $ \j -> VUM.modify vec (min i) j
  return vec

-- Usage

sieveLDSample :: IO ()
sieveLDSample = do
  let t = sieveLD 20
  print t
