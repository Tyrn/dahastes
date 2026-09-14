{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Data.Version (showVersion)
import Lib
import Options.Applicative
import Paths_dahastes (version)
import Turtle hiding (find, printf, sortBy, stderr, stdout)

-- The magic: adds -V / --version that bypasses the main parser
versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (showVersion version)
    (short 'V' <> long "version" <> help "Show version")

settingsP' :: Parser Settings
settingsP' = settingsP <**> versionOption

main :: IO ()
main = do
  args <- options description settingsP'
  copyAlbum args
