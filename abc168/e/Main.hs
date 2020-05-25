{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import           Data.Ratio
import           Data.Int
import           Data.Map.Strict            ( Map )
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Tuple
import Prelude hiding ((^))

main :: IO ()
main = do
  n <- readLn @ Int
  abs <- V.replicateM n $ (\[x, y] -> (x, y)) . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let abs' = V.map f abs
  let zeros = V.length $ V.filter isNothing abs'
  let h = Map.fromListWith (+) [ (x, toM 1) | Just x <- V.toList abs']
  print $ unM $ go h 1 - 1 + fromIntegral zeros
 where
  go h ans
    | Map.null h = ans
    | otherwise =
      let
        ((r, c), h') = Map.deleteFindMin h
        r' = (\(x, y) -> (y, -x)) r
      in case h' Map.!? r' of
        Nothing -> go h' (2^c * ans)
        Just c' -> go (Map.delete r' h') ((2^c-1 + 2^c'-1 + 1) * ans)

f :: (Int, Int) -> Maybe (Int, Int)
f (0, 0) = Nothing
f (x, y) = if y < 0 || (y == 0 && x < 0)
  then Just (-x', -y') 
  else Just (x', y')
  where
    g = gcd x y
    x' = x `div` g
    y' = y `div` g

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
