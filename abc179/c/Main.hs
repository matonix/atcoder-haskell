module Main where

import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import Data.Vector.Algorithms.Search

main :: IO ()
main = do
  x <- readLn :: IO Int
  let as = VU.enumFromN 1 x
  print $ VU.sum $ VU.map (\a -> (x - 1) `div` a) as
