{-# LANGUAGE OverloadedStrings #-}
module Atcoder.Task
  ( readTasksWithAuth
  , readTasks
  , getTasks
  , Task(..)
  ) where

import RIO hiding (to)
import RIO.List
import RIO.List.Partial
import qualified RIO.Text as T
import Text.XML (Document)
import Text.XML.Lens hiding ((<.>), name)
import qualified Network.HTTP.Simple as H
import qualified Text.HTML.DOM as DOM
import Util

data Task = Task
  { name :: Text
  , url :: String
  } deriving (Show, Eq)

-- * Exported Functions

readTasksWithAuth 
  :: Maybe ByteString
  -> Maybe ByteString 
  -> String 
  -> IO [Task]
readTasksWithAuth (Just username) (Just password) tasksUrl = do
  req <- H.parseRequest tasksUrl
  let req' = H.setRequestBasicAuth username password req
  res <- H.httpLBS req'
  return $ getTasks $ DOM.parseLBS $ H.getResponseBody res
readTasksWithAuth _ _ tasksUrl = readTasks tasksUrl

readTasks :: String -> IO [Task]
readTasks tasksUrl = do
  req <- H.parseRequest tasksUrl
  res <- H.httpLBS req
  return $ getTasks $ DOM.parseLBS $ H.getResponseBody res

getTasks :: Document -> [Task]
getTasks = dropCommonPrefix . map makeTask . docToUrls

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

dropCommonPrefix :: [Task] -> [Task]
dropCommonPrefix tasks = zipWith Task suffixes $ map url tasks
  where
    suffixes = go $ map (T.unpack . name) tasks
    go [] = []
    go tss'@(ts : tss)
      | any ((< 2) . length) tss' = map T.pack tss'
      | any (/= head ts) (map head tss) = map T.pack tss'
      | otherwise = go $ tail ts : map tail tss