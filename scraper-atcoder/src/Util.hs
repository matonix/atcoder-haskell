{-# LANGUAGE OverloadedStrings #-}
module Util where

import RIO
import qualified RIO.Text as T

getFinalPart :: String -> String
getFinalPart = reverse . takeWhile (/='/') . reverse

tgetFinalPart :: Text -> Text
tgetFinalPart = T.reverse . T.takeWhile (/='/') . T.reverse

emptyMain :: Text
emptyMain = T.unlines 
  [ "module Main where"
  , ""
  , "import Control.Applicative"
  , ""
  , "main :: IO ()"
  , "main = do"
  ]