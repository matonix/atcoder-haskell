module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import qualified Data.Set as Set
import           Control.Monad
main :: IO ()
main = do
  [n, k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  sets <- replicateM k $ do
    d <- readLn :: IO Int
    as <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return $ Set.fromList as
  let s = Set.unions sets
  print $ n - Set.size s
