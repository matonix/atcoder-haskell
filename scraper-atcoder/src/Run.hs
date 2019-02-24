{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as T
import Import
import Util

run :: RIO App ()
run = do
  contestUrl <- contest . appOptions <$> ask
  let contestName = getFinalPart contestUrl
  logInfo $ "Create contest directory: " <> fromString contestName
  cwd <- liftIO $ getCurrentDirectory
  let contestPath = cwd </> contestName
  liftIO $ createDirectory contestPath
  let tasksUrl = contestUrl </> "tasks"
  tasks <- liftIO $ readTasks tasksUrl
  forM_ tasks $ \(Task taskName taskUrl) -> do
    logInfo $ "  Create task directory: " <> display taskName
    let taskPath = contestPath </> T.unpack taskName
    liftIO $ createDirectory taskPath
    logInfo "    Create empty Main.hs"
    liftIO $ writeFileUtf8 (taskPath </> "Main.hs") emptyMain
    logInfo "    Create examples"
    logDebug $ "      Read " <> fromString taskUrl
    examples <- liftIO $ readExamples taskUrl
    logDebug $ "      Examples is here: " <> fromString (show examples)
    result <- liftIO $ writeExamples taskPath examples
    logDebug $ "      Wrote in: " <> fromString (show result)
    logInfo "    Create Makefile"
    liftIO $ writeMakefile (taskPath </> "Makefile") (taskMakefile $ length examples)
    logInfo $ "Create contest directory: " <> fromString contestName
  logInfo "Create Makefile for all tasks"
  liftIO $ writeMakefile (contestPath </> "Makefile") (tasksMakefile $ map name tasks)
  logInfo "Done."
