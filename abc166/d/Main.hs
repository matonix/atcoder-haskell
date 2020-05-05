{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
main :: IO ()
main = do
  x <- readLn @Int
  let (a, b) = solver x
  putStrLn $ unwords [show a, show b]

solver :: Int -> (Int, Int)
solver x =
  let nums  = [-200 .. 199]
      fives = VU.map (^ 5) $ VU.fromListN 400 [-200 .. 199]
  in  fst . head $ filter
        ((== x) . snd)
        [ ((a, b), fives VU.! (a + 200) - fives VU.! (b + 200)) | a <- nums, b <- nums ]
