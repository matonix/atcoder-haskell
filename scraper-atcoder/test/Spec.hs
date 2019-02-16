{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import RIO.Directory
import RIO.FilePath
import qualified RIO.ByteString.Lazy as BL
import qualified Network.HTTP.Simple as H
import qualified Text.HTML.DOM as DOM
import Util

main :: IO ()
main = defaultMain =<< test

test :: IO TestTree
test = do
  let url = "https://atcoder.jp/contests/abs/tasks/abc086_a"
  let taskHtml = "test" </> "resources" </> getFinalPart url <.> "html"
  p <- doesFileExist taskHtml
  unless p $ do
    req <- H.parseRequest url
    res <- H.httpLBS req
    BL.writeFile taskHtml $ H.getResponseBody res
  doc <- DOM.readFile taskHtml
  return $ testCase "read sample test cases" 
    $ getExamples doc @?= 
      [ Example "3 4\r\n" "Even\r\n"
      , Example "1 21\r\n" "Odd\r\n"
      ]

getFinalPart :: String -> String
getFinalPart = reverse . takeWhile (/='/') . reverse