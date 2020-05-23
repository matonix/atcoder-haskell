{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.WebDriver

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified Text.HTML.DOM                 as DOM
import           Text.XML                       ( Document )
import           Atcoder.Example
import           Atcoder.Task

myConfig :: WDConfig
myConfig =
  defaultConfig { wdCapabilities = (defaultCaps { browser = chrome }) }

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  task     <- taskTest
  task2    <- taskTestHeteroTaskname
  example  <- exampleTest
  example2 <- exampleTestRichRep
  example3 <- exampleTestOneLang
  return $ testGroup "Tests" [task, task2, example, example2]

getSourceWithCache :: FilePath -> FilePath -> IO Document
getSourceWithCache url name = do
  let path = "test" </> "resources" </> name <.> "html"
  p <- doesFileExist path
  unless p $ runSession myConfig $ do
    openPage url
    source <- getSource
    writeFileUtf8 path source
    closeSession
  DOM.readFile path

taskTest :: IO TestTree
taskTest = do
  let contestUrl = "https://atcoder.jp/contests/abc117"
  doc <- getSourceWithCache (contestUrl </> "tasks") (takeFileName contestUrl)
  return
    $   testCase "read tasks in contests"
    $   getTasks doc
    @?= [ Task "a" "https://atcoder.jp/contests/abc117/tasks/abc117_a"
        , Task "b" "https://atcoder.jp/contests/abc117/tasks/abc117_b"
        , Task "c" "https://atcoder.jp/contests/abc117/tasks/abc117_c"
        , Task "d" "https://atcoder.jp/contests/abc117/tasks/abc117_d"
        ]

taskTestHeteroTaskname :: IO TestTree
taskTestHeteroTaskname = do
  let contestUrl = "https://atcoder.jp/contests/atc001"
  doc <- getSourceWithCache (contestUrl </> "tasks") (takeFileName contestUrl)
  return
    $   testCase "read tasks in contests with hetero task names"
    $   getTasks doc
    @?= [ Task "a" "https://atcoder.jp/contests/atc001/tasks/dfs_a"
        , Task "b" "https://atcoder.jp/contests/atc001/tasks/unionfind_a"
        , Task "c" "https://atcoder.jp/contests/atc001/tasks/fft_c"
        ]

exampleTest :: IO TestTree
exampleTest = do
  let taskUrlOne = "https://atcoder.jp/contests/abs/tasks/abc086_a"
  doc <- getSourceWithCache taskUrlOne (takeFileName taskUrlOne)
  return
    $   testCase "read example test cases"
    $   getExamples doc
    @?= [Example "3 4\n" "Even\n", Example "1 21\n" "Odd\n"]

exampleTestRichRep :: IO TestTree
exampleTestRichRep = do
  let taskUrlOne = "https://atcoder.jp/contests/atc001/tasks/fft_c"
  doc <- getSourceWithCache taskUrlOne (takeFileName taskUrlOne)
  return
    $   testCase "read example test cases (japanese only / rich source code rep)"
    $   getExamples doc
    @?= [Example "4\n1 1\n2 2\n3 4\n4 8\n" "0\n1\n4\n11\n26\n36\n40\n32\n"]

exampleTestOneLang :: IO TestTree
exampleTestOneLang = do
  let taskUrlOne = "https://atcoder.jp/contests/atc001/tasks/unionfind_a"
  doc <- getSourceWithCache taskUrlOne (takeFileName taskUrlOne)
  return
    $   testCase "read example test cases (japanese only page)"
    $   getExamples doc
    @?= [Example "8 9\n0 1 2\n0 3 2\n1 1 3\n1 1 4\n0 2 4\n1 4 1\n0 4 2\n0 0 0\n1 0 0\n" "Yes\nNo\nYes\nYes\n"]
    