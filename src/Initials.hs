{-# LANGUAGE OverloadedStrings #-}

module Initials (
  initials,
  isSomeText,
  removeQuotedSubstrings,
  splitOnDots,
) where

import Data.ByteString qualified as B
import Data.Function ((&))
import Data.Text (Text)

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Text.Regex.TDFA

-- | Removes all double quoted substrings, if any, from a string.
removeQuotedSubstrings :: Text -> Text
removeQuotedSubstrings str =
  let quoteds =
        filter (\se -> not (null se) && head se == '"') $
          concat (T.unpack str =~ ("\"(\\.|[^\"\\])*\"" :: String) :: [[String]])
      cleanOfPairs =
        foldr
          (\quoted acc -> T.replace (T.pack quoted) " " acc)
          str
          quoteds
   in T.intercalate " " (T.splitOn "\"" cleanOfPairs)

isSomeText :: Text -> Bool
isSomeText = not . B.null . T.encodeUtf8

replaceAll :: Text -> Text -> Text -> Text
replaceAll enc wth txt = T.intercalate wth (T.splitOn enc txt)

splitOnDots :: Text -> [Text]
splitOnDots = T.words . replaceAll "." " "

makeInitial :: Text -> Text
makeInitial name
  | isNobiliary = T.take 1 name
  | otherwise = T.toUpper $ T.take 1 name
 where
  isNobiliary =
    elem
      name
      [ "von"
      , "фон"
      , "van"
      , "ван"
      , "der"
      , "дер"
      , "til"
      , "тиль"
      , "zu"
      , "цу"
      , "zum"
      , "цум"
      , "zur"
      , "цур"
      , "af"
      , "аф"
      , "of"
      , "из"
      , "da"
      , "да"
      , "de"
      , "де"
      , "des"
      , "дез"
      , "del"
      , "дель"
      , "den"
      , "ден"
      , "di"
      , "ди"
      , "dos"
      , "душ"
      , "дос"
      , "du"
      , "дю"
      , "la"
      , "ла"
      , "ля"
      , "le"
      , "ле"
      , "haut"
      , "от"
      , "the"
      ]

-- | Reduces a string of names to initials.
initials :: Text -> Text
initials authorsByComma =
  let
   in authorsByComma
        & removeQuotedSubstrings
        & T.splitOn ","
        & filter (isSomeText . T.strip . replaceAll "-" "" . replaceAll "." "")
        & fmap
          ( \author ->
              author
                & T.splitOn "-"
                & filter (isSomeText . T.strip . replaceAll "." "")
                & fmap
                  ( \barrel ->
                      barrel
                        & splitOnDots
                        & filter (isSomeText . T.strip)
                        & fmap
                          ( \name ->
                              name
                                & makeInitial
                          )
                        & T.intercalate "."
                  )
                & T.intercalate "-"
                & (<> ".")
          )
        & T.intercalate ","
