{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.Int
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List
import           Data.Array

main :: IO ()
main = do
  [n', k'] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let n    = fromIntegral n' :: Int64
  let k    = fromIntegral k' :: Int64
  let css  = combsWithRep [1 .. k] n'
  let gcdn = getGcd k
  let fact = getFact k
  print $ sum [ f fact cs * gcdn cs | cs <- css ]
 where
  -- counting permutation with repetition -> multinomial coefficient
  f fact xs =
    let bags  = map (fromIntegral . length) $ group xs
        total = sumMod bags
    in  fact total `div` productMod (map fact bags)
  getGcd x = foldr gcd' 0
   where
    gcd' a 0 = a
    gcd' a b = dp ! (b, a `mod` b)
    dp = listArray ((0, 0), (x, x)) [ gcd' a b | a <- [0 .. x], b <- [0 .. x] ]
  getFact x = (dp !)
   where
    fact' 1 = 1
    fact' x = (dp ! (x - 1)) *% x
    dp = listArray (1, x) $ map fact' [1 .. x]
-- https://rosettacode.org/wiki/Combinations_with_repetitions#Dynamic_Programming
combsWithRep :: [a] -> Int -> [[a]]
combsWithRep xs k = combsBySize xs !! k
 where
  combsBySize = foldr f ([[]] : repeat [])
  f x = scanl1 $ (++) . map (x :)

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
(/%) a b = a *% powerMod b (modNum - 2) modNum where
  -- SICP Power iteration
  power :: Integral a => (t -> t -> t) -> t -> a -> t -> t
  power _ _ 0 e = e
  power f x n e = power f (f x x) (div n 2) (if odd n then f x e else e)
  powerMod x n _ = power (*%) x n 1
infixl 7 /%

-- https://www.reddit.com/r/haskell/comments/mqtk6/fast_power_function/
fastpow :: Integer -> Integer -> Integer -> Integer
fastpow base exponents modulo = fastpow' (base `mod` modulo) exponents modulo 1
 where
  fastpow' _ 0 _ !r = r
  fastpow' b e m r =
    fastpow' (b * b `mod` m) (e `div` 2) m (if even e then r else r * b `mod` m)

productMod :: [Int64] -> Int64
productMod = foldr (*%) 1

sumMod :: [Int64] -> Int64
sumMod = foldr (+%) 0
