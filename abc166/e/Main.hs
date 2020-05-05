{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap


main :: IO ()
main = do
  n  <- readLn @Int
  as <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let xs = hist $ VU.toList $ VU.imap (-) as
  let ys = hist $ VU.toList $ VU.imap (+) as
  print $ sum $ HashMap.elems $ HashMap.intersectionWith (*) xs ys
-- create histogram from list
hist :: [Int] -> HashMap Int Int
hist xs = HashMap.fromListWith (+) $ zip xs $ repeat 1
