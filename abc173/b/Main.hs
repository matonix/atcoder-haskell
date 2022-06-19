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
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  bss <- V.replicateM n getLine
  let m = Map.fromListWith (+) $ V.toList $ V.zip bss $ V.replicate n (1::Int)
  putStrLn $ "AC x " ++ show (fromMaybe 0 (m Map.!? "AC"))
  putStrLn $ "WA x " ++ show (fromMaybe 0 (m Map.!? "WA"))
  putStrLn $ "TLE x " ++ show (fromMaybe 0 (m Map.!? "TLE"))
  putStrLn $ "RE x " ++ show (fromMaybe 0 (m Map.!? "RE"))
