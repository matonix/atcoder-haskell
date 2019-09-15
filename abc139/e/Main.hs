module Main where

import           Control.Monad
import           Data.Maybe
import           Data.List
import           Data.Graph
import           Debug.Trace
import qualified Data.IntMap.Strict            as M
import qualified Data.IntSet                   as S
-- import qualified Data.Vector.Unboxed.Mutable as UM
-- import           Data.Bifunctor

-- import           Data.Array
import           Data.Array.IArray

main :: IO ()
main = do
  n   <- readLn :: IO Int
  ass <- replicateM n readN
  -- let arr = listArray (1, n) as :: Array Int [Int]
  print $ solve n ass
 where
  solve n ass =
    let ts   = traceShow (g) $ length (topSort g)
    -- topSort だと topology の数がわからん気がする
        g    = buildG (0, M.size ids - 1) es
        m    = M.fromListWith (++) $ map (\(x, y) -> (x, [y])) es
        -- ss   = map head vss
        es   = concatMap (\vs -> zip vs (tail vs)) vss' :: [(Int, Int)]
        vss' = map (map (ids M.!)) vss :: [[Int]]
        ids  = mkIds $ concat vss
        vss  = zipWith
          (\x ys -> map (\y -> if x < y then 1000 * x + y else 1000 * y + x) ys)
          [1 .. n]
          ass
    in  ts

readN = map (read :: String -> Int) . words <$> getLine

mkIds :: [Int] -> M.IntMap Int
mkIds = M.fromAscList . (`zip` [0 ..]) . S.toList . S.fromList
