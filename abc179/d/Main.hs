{-# LANGUAGE BangPatterns, TypeFamilies #-}
module Main where

import           Data.Int
import           Data.Ratio
import qualified Data.Vector.Unboxing as VU
import qualified Data.Vector.Unboxing.Mutable   as VUM
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import Control.Monad
import Data.Array.Unboxed
import Data.Array.IO

main :: IO ()
main = do
  [n, k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  lrs <- replicateM k $ (\v -> [v VU.! 0 .. v VU.! 1]) . VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- let ss = concat [ replicate ((n-1)`div`x) x | x <- concat $ lrs]
  let steps = VU.fromList $ concat $ lrs
  print steps
  let items = VU.length steps
  arr <- newArray ((0,0),(items+1,n+1)) (toM 0) :: IO (IOArray (Int, Int) MInt)
  writeArray arr (0,0) 1
  forM_ [0..items-1] $ \i -> do
    forM_ [0..n] $ \j -> do
      a <- readArray arr (i+1,j)
      b <- readArray arr (i,j)
      writeArray arr (i+1,j) $! a + b
      when (j >= (steps VU.! i)) $ do
        a <- readArray arr (i+1,j)
        b <- readArray arr (i,j-(steps VU.! i))
        writeArray arr (i+1,j) $! a + multinomialCoefficient i (VU.map (n`div`) $ VU.take i steps)
  ans <- readArray arr (items,n-1)
  print $ unM ans

-- permutations avec répétition
multinomialCoefficient n ks = factorial n / VU.product (VU.map factorial ks)
  where
    factorial 0 = 0
    factorial x = (fact VU.! (x-1))
    fact = VU.scanl1 (*) $ VU.enumFromN (toM 1) 20000

-- DO NOT use constructor directly, use toM instead
newtype MInt = M { unM :: Int64 } deriving (Show, Eq, Ord)

-- If you need Unbox MInt instance, uncomment following code 
-- and use Data.Vector.Unboxing(.Mutable)
-- and enable TypeFamily

instance VU.Unboxable MInt where
  type Rep MInt = Int64

modNum :: Int64
modNum = 998244353

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
