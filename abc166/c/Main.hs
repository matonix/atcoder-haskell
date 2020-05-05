module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import           Control.Monad
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.Tuple
import qualified Data.IntMap                   as Map
import qualified Data.Set                      as Set

main :: IO ()
main = do
  [n, m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  hs     <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  abs    <-
    replicateM m $ unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let es = map (\[a, b] -> (a, b)) abs
  let es' =
        Map.fromListWith Set.union
          $  map (\(x, y) -> (x, Set.singleton y))
          $  es
          ++ map swap es
  let trues = map (isHighest hs es') [1 .. n]
  print $ length $ filter id trues

-- lookups x xs = map snd $ filter ((== x) . fst) xs
isHighest :: VU.Vector Int -> Map.IntMap (Set.Set Int) -> Int -> Bool
isHighest hs es i = case es Map.!? i of
  Just rs ->
    let hs' = Set.map (\r -> hs VU.! (r - 1)) rs
    in  Set.findMax hs' < hs VU.! (i - 1)
  Nothing -> True