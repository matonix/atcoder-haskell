{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List
import           Control.Monad

data Query = AB | AC | BC deriving (Show, Eq, Read)
data Decision = A | B | C deriving (Show, Eq)

main :: IO ()
main = do
  [n, a, b, c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ss           <- replicateM n (readLn @Query)
  pr $ go [] (a, b, c) ss

go ans _ [] = Just (reverse ans)
go ans abc (q:qs) = case get abc q of
  (0, 0, d, e) -> Nothing
  (x, 0, d, e) -> go (e:ans) (set abc q (x-1) 1) qs
  (0, y, d, e) -> go (d:ans) (set abc q 1 (y-1)) qs
  (x, y, d, e) -> case compare x y of
    GT -> go (e:ans) (set abc q (x-1) 1) qs
    LT -> go (d:ans) (set abc q 1 (y-1)) qs
    EQ -> case qs of
      [] -> go (d:ans) (set abc q (x+1) (y-1)) qs
      (r:_) -> case comp q r of
        N -> go (d:ans) (set abc q (x+1) (y-1)) qs
        L -> go (d:ans) (set abc q (x+1) (y-1)) qs
        R -> go (e:ans) (set abc q (x-1) (y+1)) qs

get (a, b, c) AB = (a, b, A, B)
get (a, b, c) AC = (a, c, A, C)
get (a, b, c) BC = (b, c, B, C)

set (a, b, c) AB x y = (x, y, c)
set (a, b, c) AC x y = (x, b, y)
set (a, b, c) BC x y = (a, x, y)

data Tri = L | R | N

comp AB AB = N
comp AB AC = L
comp AB BC = R
comp AC AB = L
comp AC AC = N
comp AC BC = R
comp BC AB = L
comp BC AC = R
comp BC BC = N

pr (Just ans) = do
  putStrLn "Yes"
  mapM_ print ans
pr Nothing = putStrLn "No"
