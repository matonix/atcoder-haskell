module Main where
import qualified Data.ByteString.Char8         as BS
import           Data.Vector.Unboxed            ( (!) )
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Debug.Trace

main :: IO ()
main = do
  n <- readLn
  s <- VU.fromList <$> getLine
  let nof c = VU.length . VU.filter (== c)
  let rgb = 'R' `nof` s * 'G' `nof` s * 'B' `nof` s
  let ixs =
        [ (i, j, k)
        | i <- [0 .. n - 1]
        , j <- [i .. n - 1]
        , let k = 2 * j - i
        , k < n
        ]
  let f v (i, j, k) = v ! i /= v ! j && v ! j /= v ! k && v ! k /= v ! i
  let comp = length (filter (f s) ixs)
  print $ rgb - comp
