{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Monad.Reader
import Lib
import Turtle hiding (find, printf, sortBy, stderr, stdout)

main :: IO ()
main = do
  args <- options description settingsP
  runReaderT runApp (Ctx args)
