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
main :: IO ()
main = do
  [n, m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  abc <- V.replicateM n $ VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  def <- V.replicateM m $ VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  
  putStrLn "hoge"
