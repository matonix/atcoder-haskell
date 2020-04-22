module Main_ where

import           Data.Array
import           Data.List
import           Prelude                 hiding ( gcd )

main :: IO ()
main = do
  x <- readLn
  let dp = solve x
  print $ sum [ dp ! (a, b, c) | a <- [1 .. x], b <- [1 .. x], c <- [1 .. x] ]
 where
  solve x = dp
   where
    dp = listArray
      ((0, 0, 0), (x, x, x))
      [ gcd a b c | a <- [0 .. x], b <- [0 .. x], c <- [0 .. x] ]
    gcd a 0 0 = a
    gcd a b 0 = dp ! (b, a `rem` b, 0)
    gcd a b c = dp ! (a, c, b `rem` c)
