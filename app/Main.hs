{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Lib
import System.IO hiding (FilePath, stderr, stdout)
import Turtle hiding (find, printf, sortBy, stderr, stdout)
import Prelude hiding (FilePath)

main :: IO ()
main = do
  args <- options description settingsP
  copyAlbum args
