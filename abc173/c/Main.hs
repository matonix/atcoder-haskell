{-# LANGUAGE ScopedTypeVariables #-}
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
import           Control.Monad                  ( replicateM )

main :: IO ()
main = do
  [h, w, k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  mat <- replicateM h BS.getLine :: IO [BS.ByteString]
  let pats = brutes [True, False] (h+w)
  print $ length $ filter id $ map (query mat h w k) pats 
 where
  query :: [BS.ByteString] -> Int -> Int -> Int -> [Bool] -> Bool
  query mat h w k pat = let
    hs = take h pat
    ws = drop h pat
    trows = zip hs mat
    cols = BS.transpose $ [ snd x | x <- trows, fst x ]
    tcols = zip ws cols
    remains = [ snd x | x <- tcols, fst x ]
    in (==k) $ BS.count '#' $ BS.concat remains

brutes :: [a] -> Int -> [[a]]
brutes x n = replicateM n x

