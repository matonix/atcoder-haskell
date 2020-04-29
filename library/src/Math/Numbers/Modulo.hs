{-# LANGUAGE BangPatterns #-}
module Math.Numbers.Modulo where

import           Data.Int

modNum :: Int64
modNum = 1000000007

modd :: Int64 -> Int64
modd a = a `mod` modNum

(+%) :: Int64 -> Int64 -> Int64
(+%) a b = modd (modd a + modd b)
infixl 6 +%

(-%) :: Int64 -> Int64 -> Int64
(-%) a b = modd (modd a - modd b + modNum)
infixl 6 -%

(*%) :: Int64 -> Int64 -> Int64
(*%) a b = modd (modd a * modd b)
infixl 7 *%

-- usable only if modNum is prime
(/%) :: Int64 -> Int64 -> Int64
(/%) a b = a *% powerMod b (modNum-2) modNum where
  -- SICP Power iteration
  power :: Integral a => (t -> t -> t) -> t -> a -> t -> t
  power _ _ 0 e = e
  power f x n e = power f (f x x) (div n 2) (if odd n then f x e else e)
  powerMod x n _ = power (*%) x n 1
infixl 7 /%

-- https://www.reddit.com/r/haskell/comments/mqtk6/fast_power_function/
fastpow :: Integer -> Integer -> Integer -> Integer
fastpow base exponents modulo = fastpow' (base `mod` modulo) exponents modulo 1
  where fastpow' _ 0 _ !r = r
        fastpow' b e m r = fastpow' (b * b `mod` m) (e `div` 2) m (if even e then r else r * b `mod` m)

productMod :: [Int64] -> Int64
productMod = foldr (*%) 1

sumMod :: [Int64] -> Int64
sumMod = foldr (+%) 0
