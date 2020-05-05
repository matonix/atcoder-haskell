{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString.Char8         as BS

main :: IO ()
main = do
  s <- BS.getLine
  BS.putStrLn $ if s == "ABC" then "ARC" else "ABC"
