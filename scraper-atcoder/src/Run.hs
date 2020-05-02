{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Run
  ( run
  )
where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text                      as T
import           Import
import           Util

run :: RIO App ()
run = do
  let baseUrl = "https://atcoder.jp/contests"
  Options {..} <- appOptions <$> ask
  logInfo $ "Create contest directory: " <> fromString contest
  cwd      <- liftIO $ getCurrentDirectory
  let contestPath = cwd </> contest
  liftIO $ createDirectory contestPath
  let tasksUrl = baseUrl </> contest </> "tasks"
  tasksAndExamples <- liftIO $ runScraper username password tasksUrl
  forM_ tasksAndExamples $ \(Task taskName _, examples) -> do
    logInfo $ "  Create task directory: " <> display taskName
    let taskPath = contestPath </> T.unpack taskName
    liftIO $ createDirectory taskPath
    logInfo "    Create empty Main.hs"
    liftIO $ writeFileUtf8 (taskPath </> "Main.hs") emptyMain
    logInfo "    Create examples"
    logDebug $ "      Examples is here: " <> fromString (show examples)
    result <- liftIO $ writeExamples taskPath examples
    logDebug $ "      Wrote in: " <> fromString (show result)
    logInfo "    Create Makefile"
    liftIO $ writeMakefile (taskPath </> "Makefile")
                           (taskMakefile $ length examples)
    logInfo $ "Create contest directory: " <> fromString contest
  logInfo "Create Makefile for all tasks"
  let tasks = map fst tasksAndExamples
  liftIO $ writeMakefile (contestPath </> "Makefile")
                         (tasksMakefile $ map taskName tasks)
  logInfo "Done."
