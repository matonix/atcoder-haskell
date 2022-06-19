module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import           Control.Monad
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM

main :: IO ()
main = do
  x <- readLn :: IO Int
  xs <- V.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let t = sieveLD 1000000
  let pfs = V.map (primeFactorS t) xs
  let pa = V.sum (V.map IntSet.size pfs) == IntSet.size (V.foldl1 IntSet.union pfs)
  let se = V.foldl1 gcd xs == 1
  putStrLn $ if pa then "pairwise coprime" else if se then "setwise coprime" else "not coprime"

-- Prime factor

type LDTable = VU.Vector Int

primeFactorS :: LDTable -> Int -> IntSet
primeFactorS t x = IntSet.fromAscList $ primeFactorRaw t x 

primeFactorRaw :: LDTable -> Int -> [Int]
primeFactorRaw _ 1 = []
primeFactorRaw t x = let f = t VU.! x in f : primeFactorRaw t (x `div` f)

-- Least Divisor
-- LD(4) = 2
-- LD(6) = 2
-- LD(35) = 5

sieveLD :: Int -> LDTable
sieveLD n = VU.create $ do
  vec <- VU.thaw $ VU.enumFromN 0 (n + 1)
  VUM.write vec 0 1
  forM_ [2 .. n] $ \i -> do
    d <- VUM.read vec i
    when (i == d) $ forM_ [2 * i, 3 * i .. n] $ \j -> VUM.modify vec (min i) j
  return vec
