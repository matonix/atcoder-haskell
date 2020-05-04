{- HLINT ignore "Redundant do" -}
module Test where

import qualified Main                          as M
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BS
import           Data.Maybe
import           Test.Hspec
import           Test.Main

captureIO :: IO () -> ByteString -> IO ProcessResult
captureIO f input = captureProcessResult (withStdin input f)

main :: IO ()
main = hspec $ do
  describe "main" $ do
