module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import           Data.Maybe
import           Control.Monad
main :: IO ()
main = do
  [y, x] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  mat <- VU.unfoldrN (x * y) (BS.uncons . BS.dropWhile isSpace) <$> BS.getContents
  let from2d = mkFrom2d x y
  let to2d = mkTo2d x y
  let inBound = mkInBound x y
  let start = to2d . fromJust $ VU.elemIndex 's' mat
  let dfs mv (i, j) = do
        VUM.write mv (from2d i j) '#'
        -- print $ show (i, j)
        res <- forM [(i+1,j), (i-1,j), (i,j+1), (i,j-1)] $ \(i', j') ->
          if inBound i' j'
          then do
            c <- VUM.read mv (from2d i' j')
            case c of
              '#' -> return False
              'g' -> return True
              _ -> dfs mv (i', j')
          else return False
        return $ or res
  mat' <- VU.thaw mat
  ans <- dfs mat' start
  putYN ans

-- usage:
-- do
--   [x, y] <- getSizeAction 
--   let from2d = mkFrom2d x y
--   let to2d = mkTo2d x y
--   let inBound = mkInBound x y
--   let v ! i j = mk2dIndexing x y
--   let v !? i j = mk2dIndexingMaybe x y
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

putYN :: Bool -> IO ()
putYN True  = putStrLn "Yes"
putYN False = putStrLn "No"
