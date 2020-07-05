module Main where

import           Control.Arrow
import           Data.List
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

main :: IO ()
main = do
  n <- readLn::IO Int
  print $ VU.sum $ VU.map (f n) $ VU.enumFromN (1::Int) n
  where
    f n k = let y = n `div` k in y * (y + 1) * k `div` 2