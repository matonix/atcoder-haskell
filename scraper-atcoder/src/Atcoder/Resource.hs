{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Atcoder.Resource
  ( files
  , templates
  , createTestCase
  , createPackage
  , lookupFiles
  )
where

import           RIO
import           Data.FileEmbed
import           Text.Mustache
import qualified RIO.Text as T

data TestCase = TestCase
  { task :: Text
  , example :: Text
  }

instance ToMustache TestCase where
  toMustache testCase =
    object ["task" ~> task testCase, "example" ~> example testCase]

newtype Package = Package
  { contest :: Text
  }

instance ToMustache Package where
  toMustache package =
    object ["contest" ~> contest package]

files :: [(FilePath, ByteString)]
files = $(embedDir "resources/files")

templates :: [(FilePath, ByteString)]
templates = $(embedDir "resources/templates")

createTestCase :: String -> Int -> IO Text
createTestCase taskId exampleId = do
  let testCase     = TestCase (T.pack taskId) (tshow exampleId)
  let testFileName = "Test.mustache"
  testTemplate <- lookupFiles testFileName templates
  case compileTemplate testFileName testTemplate of
    Left  parseError -> fail $ "createTestCase: " ++ show parseError
    Right template   -> return $ substitute template testCase

createPackage :: String -> IO Text
createPackage contestId = do
  let pakcage = Package (T.pack contestId)
  let packageFileName = "package.yaml.mustache"
  packageTemplate <- lookupFiles packageFileName templates
  case compileTemplate packageFileName packageTemplate of
    Left  parseError -> fail $ "createPackage: " ++ show parseError
    Right template   -> return $ substitute template pakcage

lookupFiles :: FilePath -> [(FilePath, ByteString)] -> IO Text
lookupFiles key dict = case lookup key dict of
  Nothing -> fail $ key ++ " not found"
  Just value -> return $ decodeUtf8Lenient value