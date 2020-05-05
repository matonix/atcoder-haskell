{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import RIO
import RIO.Directory
import RIO.FilePath
import qualified RIO.ByteString.Lazy as BL
import qualified Network.HTTP.Simple as H
import qualified Text.HTML.DOM as DOM
import Atcoder.Example
import Atcoder.Task

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  task <- taskTest
  task2 <- taskTestHeteroTaskname
  example <- exampleTest
  example2 <- exampleTestOneLang
  return $ testGroup "Tests" [task, task2, example, example2]

taskTest :: IO TestTree
taskTest = do
  let contestUrl = "https://atcoder.jp/contests/abc117"
  let tasksUrl = contestUrl </> "tasks"
  let tasksHtml = "test" </> "resources"
        </> takeFileName contestUrl <.> "html"
  p <- doesFileExist tasksHtml
  unless p $ do
    req <- H.parseRequest tasksUrl
    res <- H.httpLBS req
    BL.writeFile tasksHtml $ H.getResponseBody res
  doc <- DOM.readFile tasksHtml
  return $ testCase "read tasks in contests"
    $ getTasks doc @?=
      [ Task "a" "https://atcoder.jp/contests/abc117/tasks/abc117_a"
      , Task "b" "https://atcoder.jp/contests/abc117/tasks/abc117_b"
      , Task "c" "https://atcoder.jp/contests/abc117/tasks/abc117_c"
      , Task "d" "https://atcoder.jp/contests/abc117/tasks/abc117_d"
      ]

taskTestHeteroTaskname :: IO TestTree
taskTestHeteroTaskname = do
  let contestUrl = "https://atcoder.jp/contests/atc001"
  let tasksUrl = contestUrl </> "tasks"
  let tasksHtml = "test" </> "resources"
        </> takeFileName contestUrl <.> "html"
  p <- doesFileExist tasksHtml
  unless p $ do
    req <- H.parseRequest tasksUrl
    res <- H.httpLBS req
    BL.writeFile tasksHtml $ H.getResponseBody res
  doc <- DOM.readFile tasksHtml
  return $ testCase "read tasks in contests with hetero task names"
    $ getTasks doc @?=
      [ Task "a" "https://atcoder.jp/contests/atc001/tasks/dfs_a"
      , Task "b" "https://atcoder.jp/contests/atc001/tasks/unionfind_a"
      , Task "c" "https://atcoder.jp/contests/atc001/tasks/fft_c"
      ]

exampleTest :: IO TestTree
exampleTest = do
  let taskUrlOne = "https://atcoder.jp/contests/abs/tasks/abc086_a"
  let taskHtml = "test" </> "resources"
        </> takeFileName taskUrlOne <.> "html"
  p <- doesFileExist taskHtml
  unless p $ do
    req <- H.parseRequest taskUrlOne
    res <- H.httpLBS req
    BL.writeFile taskHtml $ H.getResponseBody res
  doc <- DOM.readFile taskHtml
  return $ testCase "read example test cases"
    $ getExamples doc @?=
      [ Example "3 4\n" "Even\n"
      , Example "1 21\n" "Odd\n"
      ]

exampleTestOneLang :: IO TestTree
exampleTestOneLang = do
  let taskUrlOne = "https://atcoder.jp/contests/atc001/tasks/fft_c"
  let taskHtml = "test" </> "resources"
        </> takeFileName taskUrlOne <.> "html"
  p <- doesFileExist taskHtml
  unless p $ do
    req <- H.parseRequest taskUrlOne
    res <- H.httpLBS req
    BL.writeFile taskHtml $ H.getResponseBody res
  doc <- DOM.readFile taskHtml
  return $ testCase "read example test cases (japanese only page)"
    $ getExamples doc @?=
      [ Example "4\n1 1\n2 2\n3 4\n4 8\n" "0\n1\n4\n11\n26\n36\n40\n32\n"
      ]