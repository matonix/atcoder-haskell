{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
import           Data.Int
import           Data.Ratio
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
-- import qualified Data.Vector.Unboxed           as VU
-- import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.Complex
import           Data.Function
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.List as L
import qualified Data.Vector.Unboxing as VU
import qualified Data.Vector.Unboxing.Mutable   as VUM

main :: IO ()
main = do
  x <- readLn :: IO Int
  as <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let ms = VU.map toM as
  print $ unM $ VU.sum $ VU.zipWith (*) ms $ VU.scanl (+) 0 ms

-- DO NOT use constructor directly, use toM instead
newtype MInt = M { unM :: Int64 } deriving (Show, Eq, Ord)

instance VU.Unboxable MInt where
  type Rep MInt = Int64

modNum :: Int64
modNum = 1000000007

toM :: Integral a => a -> MInt
toM a = M (fromIntegral a `mod` modNum)

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
