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

{- | Removes all double quoted substrings, if any, from a string.

Examples:

>>> removeQuotedSubstrings "alfa"
"alfa"
>>> removeQuotedSubstrings "\"\"ngoro\"dup\"lai \"ming\""
" ngoro lai  "
-}
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
makeInitial = T.take 1

{- | Reduces a string of names to initials.

Examples:

>>> initials " "
"."
>>> initials "John ronald reuel\tTolkien"
"J.R.R.T."
>>> initials "e. B. Sledge"
"E.B.S."
>>> initials "Apsley  Cherry-Garrard "
"A.C-G."
>>> initials "Windsor Saxe-\tCoburg - Gotha"
"W.S-C-G."
>>> initials "Elisabeth Kubler-- - Ross"
"E.K-R."
>>> initials "Fitz-Simmons Ashton-Burke Leigh"
"F-S.A-B.L."
>>> initials "Arleigh\"31-knot\"Burke  "
"A.B."
-}
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
