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
    it "input1" $ do
      input  <- BS.readFile "a/input1"
      expect <- BS.readFile "a/expect1"
      output <- captureIO M.main input
      prException output `shouldSatisfy` isNothing
      prStdout output `shouldBe` expect

    it "input2" $ do
      input  <- BS.readFile "a/input2"
      expect <- BS.readFile "a/expect2"
      output <- captureIO M.main input
      prException output `shouldSatisfy` isNothing
      prStdout output `shouldBe` expect

