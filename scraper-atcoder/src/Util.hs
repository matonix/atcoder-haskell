{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Util
  ( readExamples
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

readExamples :: String -> IO [Example]
readExamples url = do
  req <- H.parseRequest url
  res <- H.httpLBS req
  return $ getExamples $ DOM.parseLBS $ H.getResponseBody res

getExamples :: Document -> [Example]
getExamples = makeExample . makePairs . docToTexts

writeExamples :: [Example] -> IO [(FilePath, FilePath)]
writeExamples es = forM (zip ['a'..] es) $ 
  \(n, Example i e) -> do
    let iFile = [n] <.> "input"
    let eFile = [n] <.> "expect"
    writeFileUtf8 iFile i
    writeFileUtf8 eFile e
    return (iFile, eFile)

writeExamples_ :: [Example] -> IO ()
writeExamples_ = void . writeExamples

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

makePairs :: [Text] -> [(Text, Text)]
makePairs = toPair . dropWhile isInput1
  where
    isInput1 = maybe False ((/= '1') . fst) . T.uncons . T.reverse
    toPair [] = []
    toPair [_] = []
    toPair (x : y : xs) = (x, y) : toPair xs

makeExample :: [(Text, Text)] -> [Example]
makeExample =  toExample . map snd
  where
    toExample [] = []
    toExample [_] = []
    toExample (x : y : xs) = Example x y : toExample xs
