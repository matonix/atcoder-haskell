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
      input  <- BS.readFile "f/input1"
      expect <- BS.readFile "f/expect1"
      output <- captureIO M.main input
      prException output `shouldSatisfy` isNothing
      prStdout output `shouldBe` expect

    it "input2" $ do
      input  <- BS.readFile "f/input2"
      expect <- BS.readFile "f/expect2"
      output <- captureIO M.main input
      prException output `shouldSatisfy` isNothing
      prStdout output `shouldBe` expect

    it "input3" $ do
      input  <- BS.readFile "f/input3"
      expect <- BS.readFile "f/expect3"
      output <- captureIO M.main input
      prException output `shouldSatisfy` isNothing
      prStdout output `shouldBe` expect

    it "input4" $ do
      input  <- BS.readFile "f/input4"
      expect <- BS.readFile "f/expect4"
      output <- captureIO M.main input
      prException output `shouldSatisfy` isNothing
      prStdout output `shouldBe` expect

    it "input5" $ do
      input  <- BS.readFile "f/input5"
      expect <- BS.readFile "f/expect5"
      output <- captureIO M.main input
      prException output `shouldSatisfy` isNothing
      prStdout output `shouldBe` expect

    it "input6" $ do
      input  <- BS.readFile "f/input6"
      expect <- BS.readFile "f/expect6"
      output <- captureIO M.main input
      prException output `shouldSatisfy` isNothing
      prStdout output `shouldBe` expect

