{-# LANGUAGE OverloadedStrings #-}
module Atcoder.Task
  ( createTasksWithAuth
  , createTasks
  , getTasks
  , Task(..)
  )
where

import           RIO                     hiding ( to )
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
getTasks = uncurry (zipWith Task) . (docToNames &&& docToUrls)

-- * Unexported Functions

docToUrls :: Document -> [String]
docToUrls doc = 
  doc
    ^.. root
    .   entire
    ./  attributeIs "class" "table table-bordered table-striped"
    ./  el "tbody"
    ./  el "tr"
    ./  attributeIs "class" "text-center no-break"
    ./  attr "href"
    . to (T.append "https://atcoder.jp")
    . to T.unpack

docToNames :: Document -> [String]
docToNames doc =
  doc
    ^.. root
    .   entire
    ./  attributeIs "class" "table table-bordered table-striped"
    ./  el "tbody"
    ./  el "tr"
    ./  attributeIs "class" "text-center no-break"
    ./  text
    . to T.toLower
    . to T.unpack