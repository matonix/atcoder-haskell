{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

run :: RIO App ()
run = do
  url <- problem . appOptions <$> ask
  logInfo $ "Read " <> fromString url
  examples <- liftIO $ readExamples url
  logInfo $ "Examples is here: " <> fromString (show examples)
  result <- liftIO $ writeExamples examples
  logInfo $ "Wrote in: " <> fromString (show result)
