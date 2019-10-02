module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString.Char8         as BS
import           Data.List
import           Data.Char
import           Control.Monad
import           Data.Bits
import           Data.Function
import           Debug.Trace

ub = 1145141919

main :: IO ()
main = do
  [n, m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  keys   <- replicateM m $ do
    [a, b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    cs     <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    let bits = foldl' setBit zeroBits $ map pred cs
    return (a, bits)
  let full = bit n - 1
  let ans = solve full keys VU.! full
  print $ if ans == ub then -1 else ans

solve :: Int -> [(Int, Int)] -> VU.Vector Int
solve full keys = VU.create $ do
  dp <- VUM.replicate (full + 1) ub
  VUM.write dp 0 0
  forM_ [0 .. full - 1] $ \j ->
    forM_ keys $ \(ai, di) -> do
      let j_or_di = j .|. di
      candidate1 <- VUM.read dp j_or_di
      candidate2 <- VUM.read dp j
      let candidate2' = candidate2 + ai
      when (candidate1 > candidate2') $ 
        VUM.write dp j_or_di candidate2'
  return dp
      
