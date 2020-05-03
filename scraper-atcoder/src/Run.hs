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

run :: RIO App ()
run = do
  let baseUrl = "https://atcoder.jp/contests"
  Options {..} <- appOptions <$> ask
  logInfo $ "Create contest directory: " <> fromString contest
  cwd <- liftIO $ getCurrentDirectory

  let contestPath = cwd </> contest
  liftIO $ createDirectory contestPath
  logInfo $ "Copy project files"
  forM_ files $ \(fileName, content) -> do
    logInfo $ "  Create : " <> fromString fileName
    liftIO $ writeFileBinary (contestPath </> fileName) content
  packageContent <- liftIO $ createPackage contest
  liftIO $ writeFileUtf8 (contestPath </> "package.yaml") packageContent
  logInfo $ "Copy project files done."

  let tasksUrl = baseUrl </> contest </> "tasks"
  tasksAndExamples <- liftIO $ runScraper username password tasksUrl
  forM_ tasksAndExamples $ \(Task taskName _, examples) -> do
    logInfo $ "  Create task directory: " <> fromString taskName
    let taskPath = contestPath </> taskName
    liftIO $ createDirectory taskPath

    logInfo "    Create Main.hs"
    mainFile <- liftIO $ lookupFiles "Main.hs" templates
    liftIO $ writeFileUtf8 (taskPath </> "Main.hs") mainFile

    logInfo "    Create Test.hs"
    testPartial <- liftIO $ lookupFiles "Test.hs" templates
    testCases <- liftIO $ mapM (createTestCase taskName) [1 .. length examples]
    let testFile = testPartial `T.append` T.concat testCases
    liftIO $ writeFileUtf8 (taskPath </> "Test.hs") testFile

    logInfo "    Create examples"
    logDebug $ "      Examples is here: " <> displayShow examples
    result <- liftIO $ writeExamples taskPath examples
    logDebug $ "      Wrote in: " <> displayShow result

  logInfo "Done."
