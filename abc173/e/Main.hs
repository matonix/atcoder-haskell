{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Algorithms.Tim as VAT
import           Data.Ord
import           Data.Int
import           Data.Ratio

main :: IO ()
main = do
  [n, k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as' <- do
    x <- VU.thaw as
    VAT.sortBy (comparing (Down . abs)) x
    VU.freeze x  
  print as'

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
