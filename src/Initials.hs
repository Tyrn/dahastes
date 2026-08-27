{-# LANGUAGE OverloadedStrings #-}

module Initials (
  initials,
  isSomeText,
  removeQuotedSubstrings,
  splitOnDots,
) where

import Data.ByteString qualified as B
import Data.Char (isLower, isUpper, toUpper)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Text.Regex.TDFA

-- | Removes all double quoted substrings, if any, from a string.
removeQuotedSubstrings :: Text -> Text
removeQuotedSubstrings str =
  let quoteds =
        filter
          ( \case
              ('"' : _) -> True
              _ -> False
          )
          $ concat (T.unpack str =~ ("\"(\\.|[^\"\\])*\"" :: String) :: [[String]])
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
  | isNobiliary name = T.take 1 name
  | T.any (== '\'') name = apostrophed name
  | name `elem` ["Старший"] = "Ст"
  | name `elem` ["Младший"] = "Мл"
  | name `elem` singlePrefixes = name
  | T.length name > 1 = camelCaseAndOrdinary name
  | otherwise = T.singleton (toUpper (T.head name)) -- Just length < 2
 where
  singlePrefixes = ["Ст", "ст", "Sr", "Мл", "мл", "Jr"]

  apostrophed :: Text -> Text
  apostrophed n = case T.splitOn "'" n of
    -- Case: pre'post with post having multiple chars (e.g., O'Connor)
    [pre, post]
      | not (T.null post) && T.length post > 1 ->
          if isLower (T.head post) && not (T.null pre)
            then T.singleton (toUpper (T.head pre))
            else pre <> "'" <> T.singleton (T.head post)
    -- Case: 'post (e.g., 'B)
    ["", post]
      | not (T.null post) ->
          "'" <> T.singleton (T.head post)
    -- Fallback: single character or already just the first letter
    _ -> T.singleton (toUpper (T.head n))

  camelCaseAndOrdinary :: Text -> Text
  camelCaseAndOrdinary n =
    let first = T.head n
        rest = T.drop 1 n
        (prefix, suffix) = T.break isUpper rest
     in if T.null suffix
          then T.singleton (toUpper first)
          else T.cons first (prefix <> T.singleton (T.head suffix))

  isNobiliary n =
    elem
      n
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
