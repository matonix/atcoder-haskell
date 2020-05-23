module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
main :: IO ()
main = do
  [a, b, h, m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let th = 2 * pi * (fromIntegral h) / 12 + tm / 12 :: Double
      tm = 2 * pi * (fromIntegral m) / 60 :: Double
      ax = fromIntegral a * sin th
      ay = fromIntegral a * cos th
      bx = fromIntegral b * sin tm
      by = fromIntegral b * cos tm
  print $ sqrt $ (ax - bx) ^ 2 + (ay - by) ^ 2
