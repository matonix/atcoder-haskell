module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )

main :: IO ()
main = do
  x <- readLn :: IO Int
  xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ sum $ zipWith (-) (scanl1 max xs) xs
