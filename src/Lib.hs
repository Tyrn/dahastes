{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Support for Procrustes SmArT utility (audio album builder).
module Lib (
  strp,
  wrap,
  baseName,
  makeCounter,
  Counter,
  isAudioFile,
  zeroPad,
  strStripNumbers,
  cmpstrNaturally,
  setTagsToCopy,
  Settings (..),
  description,
  settingsP,
  putHeader,
  putCopy,
  putFooter,
) where

import Data.Char (toUpper)
import Data.Either.Extra
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Filesystem.Path.CurrentOS qualified as FPS
import Initials
import Sound.HTagLib
import System.IO hiding (FilePath, stderr, stdout)
import Text.Printf
import Text.Regex.TDFA
import Turtle hiding (find, printf, stderr, stdout)
import Prelude hiding (FilePath)

{- Command line parser -}

-- | Represents command line options.
data Settings = Settings
  { sVerbose :: !Bool
  , sVersion :: !Bool
  , sDropTracknumber :: !Bool
  , sStripDecorations :: !Bool
  , sFileTitle :: !Bool
  , sFileTitleNum :: !Bool
  , sSortLex :: !Bool
  , sTreeDst :: !Bool
  , sDropDst :: !Bool
  , sReverse :: !Bool
  , sOverwrite :: !Bool
  , sDryrun :: !Bool
  , sCount :: !Bool
  , sFileType :: !(Maybe Text)
  , sPrependSubdirName :: !Bool
  , sUnifiedName :: !(Maybe Text)
  , sAlbumNum :: !(Maybe Int)
  , sArtistTag :: !(Maybe Text)
  , sAlbumTag :: !(Maybe Text)
  , sSrc :: !FilePath
  , sDst :: !FilePath
  }

hi :: String
hi = "\x2728"

fw :: String
fw = "\x1f98b"

su :: String
su = "❔"

-- | Command line options definition.
settingsP :: Parser Settings
settingsP =
  Settings
    <$> switch "verbose" 'v' [i|#{hi} Unless verbose, just progress bar is shown|]
    <*> switch "version" 'V' "Show the version and exit"
    <*> switch "droptracknumber" 'd' "Do not set track numbers"
    <*> switch "stripdecorations" 's' "Strip file and directory name decorations"
    <*> switch "filetitle" 'f' "Use file name for title tag"
    <*> switch "filetitlenum" 'F' "Use numbered file name for title tag"
    <*> switch "sortlex" 'x' "Sort files lexicographically"
    <*> switch "treedst" 't' "Retain the tree structure of the source album at destination"
    <*> switch "dropdst" 'p' "Do not create destination directory"
    <*> switch "rev" 'r' "Copy files in reverse order (number one file is the last to be copied)"
    <*> switch "overwrite" 'w' "Silently remove existing destination directory"
    <*> switch "dryrun" 'y' "Without writing; trumps -w, too"
    <*> switch "count" 'c' "Just count the files"
    <*> optional (optText "filetype" 'e' "Accept only audio files of the specified type")
    <*> switch "prependsubdirname" 'i' "Prepend current subdirectory name to a file name"
    <*> optional (optText "unifiedname" 'u' [i|#{hi} Base name for everything, except for the "Artist" tag|])
    <*> optional (optInt "albumnum" 'b' "Add album number to destination")
    <*> optional (optText "artisttag" 'a' [i|#{hi} "Artist" tag|])
    <*> optional (optText "albumtag" 'm' [i|#{hi} "Album" tag|])
    <*> argPath "src" "Source directory"
    <*> argPath "dst" "Destination directory"

-- | Utility description (help screen header).
description :: Description
description =
  [i|  Dahastes a.k.a. Damastes SmArT is a CLI utility for copying subtrees containing
  supported audio files in sequence, naturally sorted. The end result is a flattened copy
  of the source subtree. "Flattened" means that only a namesake of the root source
  directory is created, where all the files get copied to, names prefixed with a serial
  number. Tag "Track Number" is set, tags "Title", "Artist", and "Album" can be replaced
  optionally. The writing process is strictly sequential: either starting with the number
  one file, or in the reversed order. This can be important for some mobile devices.
  #{hi} Really useful options. #{su} Suspicious media.|]

{- Counter, mostly global -}

-- | Represents a nonlocal counter.
type Counter = Int -> IO Int

-- | Returns a function capable of returning increasing values (counter).
makeCounter :: IO Counter
makeCounter = do
  r <- newIORef 0
  return
    ( \idx -> do
        modifyIORef r (+ idx)
        readIORef r
    )

{- Audio tags management -}

-- | Makes custom title tag
shapeTitle :: Settings -> Int -> String -> String -> Text
shapeTitle args n fileName ss =
  T.pack
    ( if sFileTitleNum args
        then printf "%d>%s" n fileName -- Add Track Number to Title
        else
          if sFileTitle args
            then fileName
            else printf "%d %s" n ss
    )

-- | Sets tags to the destination file.
setTagsToCopy :: Settings -> Int -> FilePath -> IO ()
setTagsToCopy args trackNum file
  | isJust (sArtistTag args) && isAlbumTag =
      st $
        titleSetter
          ( mkTitle $
              tt
                ( T.unpack $
                    initials artist
                      <> " - "
                      <> album
                )
          )
          <> artistSetter (mkArtist artist)
          <> albumSetter (mkAlbum album)
          <> track
  | isJust (sArtistTag args) =
      st $
        titleSetter (mkTitle $ tt $ T.unpack artist)
          <> artistSetter (mkArtist artist)
          <> track
  | isAlbumTag =
      st $
        titleSetter (mkTitle $ tt $ T.unpack album)
          <> albumSetter (mkAlbum album)
          <> track
  | otherwise = return ()
 where
  st = setTags (strp file) Nothing
  tt = shapeTitle args trackNum (strp $ baseName file)
  artist = fromMaybe "*" (sArtistTag args)
  album = case sUnifiedName args of
    Just uname -> uname
    Nothing -> fromMaybe "*" (sAlbumTag args)
  isAlbumTag = isJust (sAlbumTag args) || isJust (sUnifiedName args)
  track =
    if sDropTracknumber args
      then mempty
      else trackNumberSetter (mkTrackNumber trackNum)

{- FilePath helpers -}

{- | Extracts String From FilePath
(good until deprecated system-filepath removed).
-}
strp :: FilePath -> String
strp path = T.unpack $ fromRight "" (FPS.toText $ FPS.fromText (T.pack path))

-- | Constructs FilePath.
wrap :: String -> FilePath
wrap = fromString

-- | Returns base name plain or dotted
baseName :: FilePath -> FilePath
baseName = dropExtension . filename

{- String utilities -}

-- | Returns True in case of audio file extension.
isAudioFile :: Settings -> FilePath -> Bool
isAudioFile args file =
  let ext = case extension file of
        Just extn -> fmap toUpper extn
        Nothing -> ""
   in elem ext checkList
 where
  checkList = case sFileType args of
    Just ftype -> [dropWhile (== '.') (T.unpack $ T.toUpper ftype)]
    Nothing -> ["MP3", "M4A", "M4B", "OGG", "WMA", "FLAC"]

{- | Returns a zero-padded numeric literal.

Examples:

>>> zeroPad 3 5
"00003"
>>> zeroPad 15331 3
"15331"
-}
zeroPad :: Int -> Int -> String
zeroPad n len = printf ("%0" <> printf "%d" len <> "d") n

{- | Returns a list of integer numbers embedded in a string arguments.

Examples:

>>> strStripNumbers "ab11cdd2k.144"
[11,2,144]
>>> strStripNumbers "Ignacio Vazquez-Abrams"
[]
-}
strStripNumbers :: String -> [Int]
strStripNumbers str =
  let numbers = concat (str =~ ("[0-9]+" :: String) :: [[String]])
   in [read n :: Int | n <- numbers]

{- | If both strings contain digits, returns numerical comparison based on the numeric
values embedded in the strings, otherwise returns the standard string comparison.
The idea of the natural sort as opposed to the standard lexicographic sort is one of coping
with the possible absence of the leading zeros in 'numbers' of files or directories.

Examples:

>>> cmpstrNaturally "" ""
EQ
>>> cmpstrNaturally "2a" "10a"
LT
>>> cmpstrNaturally "alfa" "bravo"
LT
-}
cmpstrNaturally :: String -> String -> Ordering
cmpstrNaturally xx y =
  let nx = strStripNumbers xx
      ny = strStripNumbers y
   in if not (null nx) && not (null ny)
        then compare nx ny
        else compare xx y

{- Console output -}

-- | Prints the header of the output to the console.
putHeader :: Settings -> IO ()
putHeader args = do
  if sVerbose args
    then putStr ""
    else putStr "Start "

-- | Prints a single file copy info to the console.
putCopy :: Settings -> Int -> Int -> Int -> FilePath -> IO ()
putCopy args total totw n dstFile = do
  if sVerbose args
    then
      let fmt = "%" <> printf "%d" totw <> [i|d#{fw}%d %s\n|]
       in putStr (printf fmt n total (strp dstFile))
    else putStr "."

-- | Prints the footer of the output to the console.
putFooter :: Settings -> Int -> IO ()
putFooter args total = do
  if sVerbose args
    then putStr (printf "Total of %d file(s) copied\n" total)
    else putStr (printf " Done(%d)\n" total)
