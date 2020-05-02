{-# LANGUAGE OverloadedStrings #-}
module Atcoder.Scraper
  ( runScraper
  )
where

import           RIO
import qualified RIO.Text.Lazy                 as TL
import           Test.WebDriver
import qualified Text.HTML.DOM                 as DOM
import           Atcoder.Task
import           Atcoder.Example

myConfig :: WDConfig
myConfig =
  defaultConfig { wdCapabilities = (defaultCaps { browser = chrome }) }

runScraper :: Text -> Text -> FilePath -> IO [(Task, [Example])]
runScraper username password tasksUrl = runSession myConfig $ do
  setImplicitWait 1000 -- ms
  -- login phase
  openPage "https://atcoder.jp/login"
  usernameInput <- findElem (ById "username")
  sendKeys username usernameInput
  passwordInput <- findElem (ById "password")
  sendKeys password passwordInput
  loginButton <- findElem (ById "submit")
  submit loginButton
  -- move to contest page
  openPage tasksUrl
  tasks     <- getTasks . DOM.parseLT . TL.fromStrict <$> getSource
  exampless <- forM tasks $ \(Task _ url) -> do
    -- task page
    openPage url
    getExamples . DOM.parseLT . TL.fromStrict <$> getSource
  closeSession
  return $ zip tasks exampless
