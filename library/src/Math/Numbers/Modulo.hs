{-# LANGUAGE BangPatterns #-}
module Math.Numbers.Modulo where

import           Data.Int
import           Data.Ratio

-- DO NOT use constructor directry, instead: toM 
newtype MInt = M { unM :: Int64 } deriving (Show, Eq, Ord)

modNum :: Int64
modNum = 1000000007

toM :: Int64 -> MInt
toM a = M (a `mod` modNum)

instance Enum MInt where
  toEnum a = M (toEnum a)
  fromEnum a = fromEnum (unM a)

instance Num MInt where
  a + b = toM (unM a + unM b)
  a - b = toM (unM a - unM b + modNum)
  a * b = toM (unM a * unM b)
  abs a = toM (abs (unM a))
  signum a = toM (signum (unM a))
  fromInteger a = toM (fromIntegral a)

-- Safe only if modNum is prime
instance Fractional MInt where
  fromRational a = fromInteger (numerator a) / fromInteger (denominator a)
  a / b = a * powerMod b (modNum-2) modNum where
    -- SICP Power iteration
    power :: Integral a => (t -> t -> t) -> t -> a -> t -> t
    power _ _ 0 e = e
    power f x n e = power f (f x x) (div n 2) (if odd n then f x e else e)
    powerMod x n _ = power (*) x n 1

-- import Prelude hiding ((^))
(^) :: MInt -> MInt -> MInt
(^) b' e' = M $ fastpow (unM b') (unM e') modNum where
  -- https://www.reddit.com/r/haskell/comments/mqtk6/fast_power_function/
  fastpow :: Int64 -> Int64 -> Int64 -> Int64
  fastpow base exponents modulo = fastpow' (base `mod` modulo) exponents modulo 1
    where fastpow' _ 0 _ !r = r
          fastpow' b e m r = fastpow' (b * b `mod` m) (e `div` 2) m (if even e then r else r * b `mod` m)
infixr 8 ^
