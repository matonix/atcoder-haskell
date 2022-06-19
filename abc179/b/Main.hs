module Main where

import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import Data.List.Extra 

main :: IO ()
main = do
  x <- readLn :: IO Int
  ds <- VU.replicateM x $ (\v -> (v VU.! 0, v VU.! 1)) . VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  putYN $ (>=3) $ maximum $ map length $ splitOn [False] $ map (\(a, b) -> a == b) $ VU.toList ds

putYN :: Bool -> IO ()
putYN True  = putStrLn "Yes"
putYN False = putStrLn "No"