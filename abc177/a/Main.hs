module Main where
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )

main :: IO ()
main = do
  [d,t,s] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  putYN $ t * s >= d 

putYN :: Bool -> IO ()
putYN True  = putStrLn "Yes"
putYN False = putStrLn "No"
