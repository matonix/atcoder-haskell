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
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HashSet
import qualified Data.IntPSQ                   as PSQ
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Primitive
import           Control.Monad.Fix
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class

main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let g = createGridGraph n
  print $ go (n*n) ps g 0 0
 where
  go nn ps g i ans = if i == nn then ans else let
    p = ps VU.! i
    c = cost g p nn
    g' = update g p
    in go nn ps g' (i+1) (ans+c)
    where
      update g p =
        let g' = HashMap.adjust (HashSet.map (\(n, w) -> (n, 0))) p g
        in fmap (HashSet.map (\(n, w) -> if n == p then (n, 0) else (n, w))) g'
      cost g p v = dijkstraShortestPath g v p 0

type Node = Int
type Weight = Int
type AdjacencyList = HashMap Node (HashSet (Node, Weight))

getNexts :: AdjacencyList -> Node -> [(Node, Weight)]
getNexts g n = case HashMap.lookup n g of
  Nothing -> []
  Just xs -> HashSet.toList xs

dijkstraShortestPath :: AdjacencyList -> Node -> Node -> Node -> Weight
dijkstraShortestPath graph maxNode source target = runST $ do
  dist <- VUM.replicate (maxNode+1) (maxBound-1::Weight)
  VUM.write dist source 0
  d <- VU.freeze dist
  let q = VU.ifoldl' (\q' i p -> PSQ.insert i p () q') PSQ.empty (d ::VU.Vector Int)
  d' <- VU.thaw d
  go graph d' target q
  where
    go :: PrimMonad m => AdjacencyList -> VUM.MVector (PrimState m) Int -> Node -> PSQ.IntPSQ Int () -> m Weight
    go g d t q =
      case PSQ.minView q of
        Nothing -> VUM.read d t
        Just (u, _, _, q') -> go2 (getNexts g u) g d t u q'
    go2 [] g d t u q = go g d t q
    go2 ((v, c):ns) g d t u q = do
      du <- VUM.read d u
      dv <- VUM.read d v
      let alt = du + c
      if alt < dv then do
        VUM.write d v alt
        let q' = PSQ.insert v alt () q
        go2 ns g d t u q'
      else go2 ns g d t u q

createGridGraph :: Int -> AdjacencyList
createGridGraph n = let
  a = HashMap.fromList [(i+j*n, HashSet.singleton (i+j*n+1, 1::Int)) | i <- [1..n-1], j <- [0..n-1]]
  b = HashMap.fromList [(i+j*n+1, HashSet.singleton (i+j*n, 1)) | i <- [1..n-1], j <- [0..n-1]]
  c = HashMap.fromList [(i+j*n+n, HashSet.singleton (i+j*n, 1)) | i <- [1..n], j <- [0..n-2]]
  d = HashMap.fromList [(i+j*n, HashSet.singleton (i+j*n+n, 1)) | i <- [1..n], j <- [0..n-2]]
  e = HashMap.fromList [(i, HashSet.singleton (0, 0)) | i <- [1..n]]
  f = HashMap.fromList [(i+n*(n-1), HashSet.singleton (0, 0)) | i <- [1..n]]
  g = HashMap.fromList [(i*n-(n-1), HashSet.singleton (0, 0)) | i <- [1..n]]
  h = HashMap.fromList [(i*n, HashSet.singleton (0, 0)) | i <- [1..n]]
  in foldl' (HashMap.unionWith HashSet.union) HashMap.empty [a,b,c,d,e,f,g,h]

-- usage:
-- do
--   [x, y] <- getSizeAction 
--   let from2d = mkFrom2d x y
--   let to2d = mkTo2d x y
--   let inBound = mkInBound x y
--   let (!) v i j  = mk2dIndexing x y
--   let (!?) v i j = mk2dIndexingMaybe x y
mkFrom2d :: Int -> Int -> Int -> Int -> Int
mkFrom2d x y i j = i + x * j

mkTo2d :: Int -> Int -> Int -> (Int, Int)
mkTo2d x y idx = (idx `mod` x, idx `div` x)

mkInBound :: Int -> Int -> Int -> Int -> Bool
mkInBound x y i j = 0 <= i && i < x && 0 <= j && j < y

mk2dIndexing :: VG.Vector v a => Int -> Int -> v a -> Int -> Int -> a
mk2dIndexing x y v i j = v VG.! mkFrom2d x y i j

mk2dIndexingMaybe :: VG.Vector v a => Int -> Int -> v a -> Int -> Int -> Maybe a
mk2dIndexingMaybe x y v i j
  | 0 <= i && i < x && 0 <= j && j < y = Just $ mk2dIndexing x y v i j
  | otherwise                          = Nothing
