{-# LANGUAGE TypeApplications #-}
module Main where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  let im = foldr (\tri m -> IntMap.insertWith (+) (f tri) (g tri) m) IntMap.empty $ combsWithRep [1..100] 3
  mapM_ (\i -> print @Int $ fromMaybe 0 (im IntMap.!? i)) [1..n]
  
f [x, y, z] = x*x + y*y + z*z + x*y + y*z + z*x
g [x, y, z]
  | x == y && y == z = 1
  | x /= y && y /= z && z /= x = 6
  | otherwise = 3

combsWithRep :: [a] -> Int -> [[a]]
combsWithRep xs k = combsBySize xs !! k
 where
  combsBySize = foldr f ([[]] : repeat [])
  f x = scanl1 $ (++) . map (x :)
