{- HLINT ignore "Redundant do" -}
module Test where

import qualified Main                          as M
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BS
import           Test.Hspec
import           Test.Main

captureIO :: IO () -> ByteString -> IO ByteString
captureIO f input = prStdout <$> captureProcessResult (withStdin input f)

main :: IO ()
main = hspec $ do
  describe "main" $ do
