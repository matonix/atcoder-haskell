{-# LANGUAGE OverloadedStrings #-}
module Atcoder.Task
  ( createTasksWithAuth
  , createTasks
  , getTasks
  , getFinalPart
  , Task(..)
  )
where

import           RIO                     hiding ( to )
import           RIO.List.Partial
import qualified RIO.Text                      as T
import           Text.XML                       ( Document )
import           Text.XML.Lens           hiding ( (<.>)
                                                , name
                                                )
import qualified Network.HTTP.Simple           as H
import qualified Text.HTML.DOM                 as DOM

data Task = Task
  { taskName :: String
  , taskUrl :: String
  } deriving (Show, Eq)

-- * Exported Functions

createTasksWithAuth
  :: Maybe ByteString -> Maybe ByteString -> String -> IO [Task]
createTasksWithAuth (Just username) (Just password) tasksUrl = do
  req <- H.parseRequest tasksUrl
  let req' = H.setRequestBasicAuth username password req
  res <- H.httpLBS req'
  return $ getTasks $ DOM.parseLBS $ H.getResponseBody res
createTasksWithAuth _ _ tasksUrl = createTasks tasksUrl

createTasks :: String -> IO [Task]
createTasks tasksUrl = do
  req <- H.parseRequest tasksUrl
  res <- H.httpLBS req
  return $ getTasks $ DOM.parseLBS $ H.getResponseBody res

getTasks :: Document -> [Task]
getTasks = dropCommonPrefix . map makeTask . docToUrls

-- * Unexported Functions

docToUrls :: Document -> [Text]
docToUrls doc =
  doc
    ^.. root
    .   entire
    ./  attributeIs "class" "table table-bordered table-striped"
    ./  el "tbody"
    ./  el "tr"
    ./  attributeIs "class" "text-center no-break"
    ./  attr "href"

makeTask :: Text -> Task
makeTask relPath = let rp = T.unpack relPath in
  Task (getFinalPart rp) ("https://atcoder.jp" ++ rp)

dropCommonPrefix :: [Task] -> [Task]
dropCommonPrefix tasks = zipWith Task suffixes $ map taskUrl tasks
 where
  suffixes = go $ map taskName tasks
  go [] = []
  go tss'@(ts : tss) | any ((< 2) . length) tss' = tss'
                     | any (/= head ts) (map head tss) = tss'
                     | otherwise = go $ tail ts : map tail tss

getFinalPart :: String -> String
getFinalPart = reverse . takeWhile (/='/') . reverse
