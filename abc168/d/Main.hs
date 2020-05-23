module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HashSet
import           Data.Tuple                     ( swap )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )
import           Data.Maybe                     ( fromJust )
import Debug.Trace

type Graph = HashMap Int (HashSet Int)

main :: IO ()
main = do
  [n, m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  abs <- VU.replicateM m $ (\[x, y] -> (x, y)) . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let g = VU.foldr' f HashMap.empty $ abs VU.++ VU.map swap abs
  let ans = IntMap.toAscList $ go g Seq.empty 1 HashSet.empty IntMap.empty
  putYN True
  -- print ans
  mapM_ (print . snd) $ tail ans

go :: Graph -> Seq Int -> Int -> HashSet Int -> IntMap Int -> IntMap Int
go graph queue node visited ans = let
  nexts = HashSet.toList $ HashSet.difference (fromJust $ HashMap.lookup node graph) visited
  ans' = foldl' f ans nexts where
    f a next = IntMap.insert next node a
  visited' = foldl' f visited nexts where
    f v next = HashSet.insert next v
  queue' = queue Seq.>< Seq.fromList nexts
  in case Seq.viewl queue' of
    Seq.EmptyL -> ans'
    x Seq.:< queue'' -> go graph queue'' x visited' ans'

f :: (Int, Int) -> Graph -> Graph
f (a, b) = HashMap.insertWith HashSet.union a (HashSet.singleton b)

putYN :: Bool -> IO ()
putYN True  = putStrLn "Yes"
putYN False = putStrLn "No"