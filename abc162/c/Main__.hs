module Main__ where

import           Data.Array
import           Data.List

main :: IO ()
main = do
  x <- readLn
  let cs = combsWithRep [1 .. x] 3
  let g  = dp x
  print $ sum [ f a b c * g a b c | [a, b, c] <- cs ]
 where
  f a b c | a == b && b == c = 1
          | a == b || b == c = 3
          | otherwise        = 6
  dp x = g
   where
    g a b c = a `gcd'` b `gcd'` c
    gcd' a 0 = a
    gcd' a b = dp' ! (b, a `mod` b)
    dp' =
      listArray ((0, 0), (x, x)) [ gcd' a b | a <- [0 .. x], b <- [0 .. x] ]

-- https://rosettacode.org/wiki/Combinations_with_repetitions#Dynamic_Programming
combsWithRep :: [a] -> Int -> [[a]]
combsWithRep xs k = combsBySize xs !! k
 where
  combsBySize = foldr f ([[]] : repeat [])
  f x = scanl1 $ (++) . map (x :)
