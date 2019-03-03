{-# LANGUAGE OverloadedStrings #-}
module Atcoder.Example
  ( readExamples
  , createExamplesWithAuth
  , createExamples
  , getExamples
  , writeExamples
  , writeExamples_
  , Example(..)
  ) where

import RIO hiding (to)
import qualified RIO.Text as T
import RIO.FilePath
import Text.XML (Document)
import Text.XML.Lens hiding ((<.>))
import qualified Network.HTTP.Simple as H
import qualified Text.HTML.DOM as DOM

data Example = Example
  { input :: Text
  , expect :: Text
  } deriving (Show, Eq)

-- * Exported Functions

readExamples :: FilePath -> IO [Example]
readExamples filepath = getExamplesLocal <$> DOM.readFile filepath

createExamplesWithAuth
  :: Maybe ByteString
  -> Maybe ByteString
  -> String
  -> IO [Example]
createExamplesWithAuth (Just username) (Just password) examplesUrl = do
  req <- H.parseRequest examplesUrl
  let req' = H.setRequestBasicAuth username password req
  res <- H.httpLBS req'
  return $ getExamples $ DOM.parseLBS $ H.getResponseBody res
createExamplesWithAuth _ _ examplesUrl = createExamples examplesUrl

createExamples :: String -> IO [Example]
createExamples url = do
  req <- H.parseRequest url
  res <- H.httpLBS req
  return $ getExamples $ DOM.parseLBS $ H.getResponseBody res

getExamples :: Document -> [Example]
getExamples = makeExample . makePairs . map toLF . docToTexts

getExamplesLocal :: Document -> [Example]
getExamplesLocal = makeExample . makePairs . map toLF . docToTexts

writeExamples :: FilePath -> [Example] -> IO [(FilePath, FilePath)]
writeExamples fp es = forM (zip [(1::Int)..] es) $
  \(n, Example i e) -> do
    let iFile = fp </> show n <.> "input"
    let eFile = fp </> show n <.> "expect"
    writeFileUtf8 iFile i
    writeFileUtf8 eFile e
    return (iFile, eFile)

writeExamples_ :: FilePath -> [Example] -> IO ()
writeExamples_ fp = void . writeExamples fp

-- * Unexported Functions

docToTexts :: Document -> [Text]
docToTexts doc = doc
  ^.. root
  . entire
  ./ attributeIs "class" "lang-ja"
  ./ attributeIs "class" "part"
  ./ el "section"
  ./ (el "h3" <> el "pre")
  . text

toLF :: Text -> Text
toLF = T.filter (/= '\r')

makePairs :: [Text] -> [(Text, Text)]
makePairs = toPair . dropWhile isInput1
  where
    isInput1 = maybe False ((/= '1') . fst) . T.uncons . T.stripStart . T.reverse
    toPair [] = []
    toPair [_] = []
    toPair (x : y : xs) = (x, y) : toPair xs

makeExample :: [(Text, Text)] -> [Example]
makeExample =  toExample . map snd
  where
    toExample [] = []
    toExample [_] = []
    toExample (x : y : xs) = Example x y : toExample xs
