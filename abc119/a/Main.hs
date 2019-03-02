{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Control.Applicative

main :: IO ()
main = do
  t <- B.getLine
  B.putStrLn $ if p t then "Heisei" else "TBD"

p t = let
  Just (y, t') = B.readInt t
  Just (m, t'') = B.readInt $ B.drop 1 t'
  Just (d, _) = B.readInt $ B.drop 1 t''
  in if
    | y < 2019 -> True
    | y > 2019 -> False
    | m < 4 -> True
    | m > 4 -> False
    | d <= 30 -> True
    | otherwise -> False
