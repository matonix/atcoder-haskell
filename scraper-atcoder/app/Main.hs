{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_scraper_atcoder

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_scraper_atcoder.version)
    "The scraper for atcoder"
    ""
    (Options
      <$> switch 
        ( long "verbose"
        <> short 'v'
        <> help "Verbose output?"
        )
      <*> strArgument 
        ( metavar "URL" 
        <> help "Problem URL"
        )
      <*> (optional $ strOption
        ( long "username"
        <> short 'u'
        <> help "Username"
        <> metavar "USERNAME"
        ))
      <*> (optional $ strOption
        ( long "password"
        <> short 'p'
        <> help "Password"
        <> metavar "PASSWORD"
        ))
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
