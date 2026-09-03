{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Lib
import Turtle hiding (find, printf, sortBy, stderr, stdout)

main :: IO ()
main = do
  args <- options description settingsP
  copyAlbum args
