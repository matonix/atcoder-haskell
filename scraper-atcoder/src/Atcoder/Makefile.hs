{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Atcoder.Makefile 
  ( taskMakefile
  , tasksMakefile
  , module Data.Makefile.Render
  ) where

import RIO
import Data.Makefile
import Data.Makefile.Render

taskMakefile :: Int -> Makefile
taskMakefile n = Makefile
  { entries = 
    [ Rule (Target ".PHONY") [Dependency "Main"] []
    , Rule (Target "all") [Dependency "Main"] $ concatMap makeCommand [1..n]
    , OtherLine ""
    , Rule (Target "Main") [] [Command "stack ghc --resolver ghc-7.10.3 -- -O2 Main.hs"
    , Command "rm Main.hi Main.o"]
    , OtherLine ""
    , Rule (Target ".PHONY") [Dependency "clean"] []
    , Rule (Target "clean") [] [Command "rm -f Main *.output"]
    ]
  }
  where
    makeCommand :: Int -> [Command]
    makeCommand i' = let i = tshow i' in
      [ Command $ "./Main < " <> i <> ".input > " <> i <> ".output"
      , Command $ "diff " <> i <> ".expect " <> i <> ".output"
      ]

tasksMakefile :: [Text] -> Makefile
tasksMakefile names = Makefile 
  { entries = 
    [ Rule (Target ".PHONY") (map Dependency names) [] ]
    ++ map makeRule names ++
    [ OtherLine ""
    , Rule (Target "clean") [] (map makeCommand names)
    ]
  }
  where
    makeRule :: Text -> Entry
    makeRule name = Rule (Target name) [] [Command $ "cd " <> name <> " && make"]
    makeCommand :: Text -> Command
    makeCommand name = Command $ "cd " <> name <> " && make clean"