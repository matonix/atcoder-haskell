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
    it "input1" $ do
      input  <- BS.readFile "c/input1"
      expect <- BS.readFile "c/expect1"
      output <- captureIO M.main input
      output `shouldBe` expect

    it "input2" $ do
      input  <- BS.readFile "c/input2"
      expect <- BS.readFile "c/expect2"
      output <- captureIO M.main input
      output `shouldBe` expect

    it "input3" $ do
      input  <- BS.readFile "c/input3"
      expect <- BS.readFile "c/expect3"
      output <- captureIO M.main input
      output `shouldBe` expect
