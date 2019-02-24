{-# LANGUAGE OverloadedStrings #-}
module Atcoder.Task
  ( readTasks
  , getTasks
  , Task(..)
  ) where

import RIO hiding (to)
import qualified RIO.Text as T
import Text.XML (Document)
import Text.XML.Lens hiding ((<.>))
import qualified Network.HTTP.Simple as H
import qualified Text.HTML.DOM as DOM
import Util

data Task = Task
  { name :: Text
  , url :: String
  } deriving (Show, Eq)

-- * Exported Functions

readTasks :: String -> IO [Task]
readTasks tasksUrl = do
  req <- H.parseRequest tasksUrl
  res <- H.httpLBS req
  return $ getTasks $ DOM.parseLBS $ H.getResponseBody res

getTasks :: Document -> [Task]
getTasks = map makeTask . docToUrls

-- * Unexported Functions

docToUrls :: Document -> [Text]
docToUrls doc = doc 
  ^.. root 
  . entire 
  ./ attributeIs "class" "table table-bordered table-striped" 
  ./ el "tbody" 
  ./ el "tr"
  ./ attributeIs "class" "text-center no-break" 
  ./ attr "href"

makeTask :: Text -> Task
makeTask relPath = let
  taskUrl = "https://atcoder.jp" ++ T.unpack relPath
  taskName = tgetFinalPart relPath
  in Task taskName taskUrl