{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import RIO.Directory
import RIO.FilePath
import qualified RIO.ByteString.Lazy as BL
import qualified Network.HTTP.Simple as H
import qualified Text.HTML.DOM as DOM
import Atcoder.Example
import Atcoder.Task
import Util

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  task <- taskTest
  example <- exampleTest
  return $ testGroup "Tests" [task, example]

taskTest :: IO TestTree
taskTest = do
  let contestUrl = "https://atcoder.jp/contests/abc117"
  let tasksUrl = contestUrl </> "tasks"
  let tasksHtml = "test" </> "resources"
        </> getFinalPart contestUrl <.> "html"
  p <- doesFileExist tasksHtml
  unless p $ do
    req <- H.parseRequest tasksUrl
    res <- H.httpLBS req
    BL.writeFile tasksHtml $ H.getResponseBody res
  doc <- DOM.readFile tasksHtml
  return $ testCase "read tasks in contests"
    $ getTasks doc @?=
      [ Task "abc117_a" "https://atcoder.jp/contests/abc117/tasks/abc117_a"
      , Task "abc117_b" "https://atcoder.jp/contests/abc117/tasks/abc117_b"
      , Task "abc117_c" "https://atcoder.jp/contests/abc117/tasks/abc117_c"
      , Task "abc117_d" "https://atcoder.jp/contests/abc117/tasks/abc117_d"
      ]

exampleTest :: IO TestTree
exampleTest = do
  let taskUrl = "https://atcoder.jp/contests/abs/tasks/abc086_a"
  let taskHtml = "test" </> "resources"
        </> getFinalPart taskUrl <.> "html"
  p <- doesFileExist taskHtml
  unless p $ do
    req <- H.parseRequest taskUrl
    res <- H.httpLBS req
    BL.writeFile taskHtml $ H.getResponseBody res
  doc <- DOM.readFile taskHtml
  return $ testCase "read example test cases"
    $ getExamples doc @?=
      [ Example "3 4\n" "Even\n"
      , Example "1 21\n" "Odd\n"
      ]
