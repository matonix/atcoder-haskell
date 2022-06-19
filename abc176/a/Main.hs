module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )

main :: IO ()
main = do
  [n, x, t] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ let (d, m) = n `divMod` x in if m == 0 then d * t else (d + 1) * t
