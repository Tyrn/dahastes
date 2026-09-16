{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Support for Procrustes SmArT utility (audio album builder).
module Lib (
  cmpstrNaturally,
  humanFine,
  Settings (..),
  description,
  settingsP,
  copyAlbum,
) where

import Control.Foldl qualified as FL

import Control.Monad.Extra
import Data.Char (toUpper)
import Data.IORef
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Version (showVersion)
import Initials
import Paths_dahastes (version)
import Sound.HTagLib
import System.Directory.OsPath.Streaming (getDirectoryContentsRecursive)
import System.IO hiding (stderr, stdout)
import System.OsPath qualified as OsPath
import System.PosixCompat.Files qualified as Posix
import Text.Printf
import Text.Regex.TDFA
import Turtle hiding (find, printf, sortBy, stderr, stdout)
import Prelude

{- Command line parser -}

-- | Represents command line options.
data Settings = Settings
  { sVerbose :: !Bool
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

ar :: String
ar = "\x1f4a5" -- Danger

hi :: String
hi = "\x2728" -- Feature

fw :: String
fw = "\x1f98b" -- Grace

su :: String
su = "❔" -- Doubt

-- | Command line options definition.
settingsP :: Parser Settings
settingsP =
  Settings
    <$> switch "verbose" 'v' [i|#{hi} Unless verbose, just progress bar is shown|]
    <*> switch "drop-tracknumber" 'd' "Do not set track numbers"
    <*> switch "strip-decorations" 's' "Strip file and directory name decorations"
    <*> switch "file-title" 'f' "Use file name for title tag"
    <*> switch "file-title-num" 'F' "Use numbered file name for title tag"
    <*> switch "sort-lex" 'x' "Sort files lexicographically"
    <*> switch "tree-dst" 't' "Retain the tree structure of the source album at destination"
    <*> switch "drop-dst" 'p' "Do not create destination directory"
    <*> switch "reverse" 'r' "Copy files in reverse order (number one file is the last to be copied)"
    <*> switch "overwrite" 'w' [i|#{ar} Silently remove existing destination directory|]
    <*> switch "dry-run" 'y' "Without writing; trumps -w, too"
    <*> switch "count" 'c' "Just count the files"
    <*> optional (optText "file-type" 'e' "Accept only audio files of the specified type")
    <*> switch "prepend-subdir-name" 'i' "Prepend current subdirectory name to a file name"
    <*> optional (optText "unified-name" 'u' [i|#{hi} Base name for everything, except for the "Artist" tag|])
    <*> optional (optInt "album-num" 'b' "Add album number to destination")
    <*> optional (optText "artist" 'a' [i|#{hi} "Artist" tag|])
    <*> optional (optText "album" 'm' [i|#{hi} "Album" tag|])
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
  #{hi} Really useful options. #{su} Suspicious media.
  v#{showVersion version}|]

{- | Counts audio files and sums their sizes recursively.
Returns (count, totalBytes).
-}
treeCount :: Settings -> IO (Int, Integer)
treeCount args = do
  src <- realpath (sSrc args)
  rootPath <- OsPath.encodeUtf src
  entries <- getDirectoryContentsRecursive rootPath
  foldM (step rootPath) (0, 0) entries
 where
  step rootPath (cnt, total) (entryPath, _fileType) = do
    let fullPath = rootPath OsPath.</> entryPath
    path <- OsPath.decodeUtf fullPath
    if isAudioFile args path
      then do
        status <- Posix.getFileStatus path
        let size = fromIntegral (Posix.fileSize status) :: Integer
        return (cnt + 1, total + size)
      else return (cnt, total)

-- On Windows, use System.Directory:
-- getFileSize = System.Directory.getFileSize

-- -- | Serves the list of all audio files in the source directory.
-- treeList :: Settings -> IO [FilePath]
-- treeList args = do
--   lst <- fold (lstree (sSrc args)) FL.list
--   return $ filter (isAudioFile args) lst

-- Builds compare function according to options (for dirList only)
makeCompare :: Settings -> (FilePath -> FilePath -> Ordering)
makeCompare args =
  let path = dropExtension
      cmp =
        if sSortLex args
          then \xx y -> compare (path xx) (path y)
          else \xx y -> cmpstrNaturally (path xx) (path y)
   in if sReverse args
        then flip cmp
        else cmp

{- | Serves the list of directories and the list of audio files
of a given parent directory (immediate offspring).
-}
dirList :: Settings -> FilePath -> IO ([FilePath], [FilePath])
dirList args src = do
  let cmp = makeCompare args
  list <- fold (ls src) FL.list
  (dirs, files) <- partitionM testdir list
  return (sortBy cmp dirs, sortBy cmp $ filter (isAudioFile args) files)

-- | Makes a file name prefix out of the Artist Tag, if there is any.
artistPrefix :: Settings -> String
artistPrefix args =
  maybe "" T.unpack (sArtistTag args)

-- | Makes destination file path.
shapeDst :: Settings -> FilePath -> Int -> Int -> FilePath -> FilePath -> FilePath
shapeDst args dstRoot totw n dstStep srcFile =
  let prefx =
        if sStripDecorations args && isNothing (sUnifiedName args)
          then ""
          else zeroPad n totw <> "-"
      name = case sUnifiedName args of
        Just uName -> T.unpack uName <> " - " <> artistPrefix args
        Nothing -> baseName srcFile
      ext = case extension srcFile of
        Just extn -> "." <> extn
        Nothing -> ""
   in dstRoot </> dstStep </> fromString (prefx <> name <> ext)

-- | Makes one copy from source to destination directory.
copyFile :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> IO ()
copyFile args dstRoot total totw counter dstStep srcFile = do
  next <- counter 1
  let n = if sReverse args then total - next + 1 else next
  let dst = shapeDst args dstRoot totw n dstStep srcFile
  cp srcFile dst
  setTagsToCopy args n dst
  putCopy args total totw n dst

-- | Walks the source tree, recreates source tree at destination.
traverseTreeDst :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> IO ()
traverseTreeDst args dstRoot total totw counter dstStep srcDir = do
  (dirs, files) <- dirList args srcDir

  let walk dir = do
        let step = dstStep </> filename dir -- dir has NO trailing slash!
        mkdir (dstRoot </> step)
        traverseTreeDst args dstRoot total totw counter step dir

  mapM_ walk dirs
  mapM_ (copyFile args dstRoot total totw counter dstStep) files

-- | Walks the source tree.
traverseFlatDst :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> IO ()
traverseFlatDst args dstRoot total totw counter srcDir = do
  (dirs, files) <- dirList args srcDir
  mapM_ (traverseFlatDst args dstRoot total totw counter) dirs
  mapM_ (copyFile args dstRoot total totw counter "") files

-- | Walks the source tree backwards.
traverseFlatDstR :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> IO ()
traverseFlatDstR args dstRoot total totw counter srcDir = do
  (dirs, files) <- dirList args srcDir
  mapM_ (copyFile args dstRoot total totw counter "") files
  mapM_ (traverseFlatDstR args dstRoot total totw counter) dirs

-- | Fires the files into the already existing destination directory.
traverseAlbum :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> IO ()
traverseAlbum args execDst total totWidth counter src = do
  putHeader args
  if sTreeDst args
    then traverseTreeDst args execDst total totWidth counter "" src
    else
      if sReverse args
        then traverseFlatDstR args execDst total totWidth counter src
        else traverseFlatDst args execDst total totWidth counter src
  putFooter args total

-- | Copies the album.
copyAlbum :: Settings -> IO ()
copyAlbum args = do
  (total, byteCount) <- treeCount args

  let totWidth = length $ show total
  counter <- makeCounter

  src <- realpath (sSrc args)
  dst <- realpath (sDst args)

  if sCount args
    then do
      printf "Files: %d, total size: %s\n" total (humanFine byteCount)
      exit ExitSuccess
    else return ()

  let srcName = dirname src -- src HAS a trailing slash!
  let albumNum = case sAlbumNum args of
        Just num -> zeroPad num 2 <> "-"
        Nothing -> ""
  let baseDst = case sUnifiedName args of
        Just uname ->
          albumNum
            <> artistPrefix args
            <> " - "
            <> T.unpack uname
        Nothing -> albumNum <> srcName
  let execDst = dst </> if sDropDst args then "" else baseDst

  if sDropDst args
    then traverseAlbum args execDst total totWidth counter src
    else do
      exists <- testdir execDst
      if exists
        then
          if sOverwrite args
            then do
              rmtree execDst
              mkdir execDst
              traverseAlbum args execDst total totWidth counter src
            else
              printf "Destination directory \"%s\" already exists\n" execDst
        else do
          mkdir execDst
          traverseAlbum args execDst total totWidth counter src

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
  st = setTags file Nothing
  tt = shapeTitle args trackNum (baseName file)
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
    Nothing -> ["MP3", "M4A", "M4B", "OGG", "WMA", "FLAC", "OPUS", "APE", "WAV"]

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

{- | Human-readable byte count, nicely rounded.

>>> humanFine 42
"42"
>>> humanFine 1800
"2kB"
>>> humanFine 123456789
"117.7MB"
-}
humanFine :: Integer -> String
humanFine bytes
  | bytes > 1 =
      let trueExp = integerLogBase 1024 bytes
          unitIdx = min trueExp (length unitList - 1)
          quotient = fromIntegral bytes / (1024 ^ unitIdx :: Double)
          (unitName, numDecimals) = unitList !! unitIdx
       in printf ("%." ++ show numDecimals ++ "f%s") quotient unitName
  | bytes == 0 = "0"
  | bytes == 1 = "1"
  | otherwise = "humanFine error; bytes: " ++ show bytes
 where
  unitList :: [(String, Int)]
  unitList =
    [ ("", 0)
    , ("kB", 0)
    , ("MB", 1)
    , ("GB", 2)
    , ("TB", 2)
    , ("PB", 2)
    , ("EB", 2)
    , ("ZB", 2)
    , ("YB", 2)
    ]

  integerLogBase :: Integer -> Integer -> Int
  integerLogBase b n
    | n < b = 0
    | otherwise = 1 + integerLogBase b (n `div` b)

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
       in putStr (printf fmt n total dstFile)
    else putStr "."

-- | Prints the footer of the output to the console.
putFooter :: Settings -> Int -> IO ()
putFooter args total = do
  if sVerbose args
    then putStr (printf "Total of %d file(s) copied\n" total)
    else putStr (printf " Done(%d)\n" total)
