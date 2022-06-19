module Main where

import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.ByteString.Char8         as BS

import Data.Bits

main :: IO ()
main = do
  n <- readLn :: IO Int
  x <- VU.reverse . VU.unfoldrN n BS.uncons <$> BS.getLine
  let x' = mkBit x
  let xs = [ complementBit x' i | i <- reverse [0..n-1] ]
  mapM_ (print . f) xs

f :: Integer -> Integer
f = g 0
  where
    g :: Integer -> Integer -> Integer
    g c 0 = c
    g c x = g (c+1) (x `mod` fromIntegral (popCount x))

mkBit :: VU.Vector Char -> Integer
mkBit = VU.ifoldl' (\bit i v -> if v == '1' then setBit bit i else bit) zeroBits
