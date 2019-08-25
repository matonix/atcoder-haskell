module Main where

import Debug.Trace

main :: IO ()
main = do
  [m, d] <- map (read :: String -> Int) . words <$> getLine
  print $ length $ filter p [ (mm, dd) | mm <- [1 .. m], dd <- [1 .. d] ]
 where
  p (mm, dd) =
    let d1  = dd `div` 10
        d10 = dd `mod` 10
    in  d1 >= 2 && d10 >= 2 && d1 * d10 == mm
