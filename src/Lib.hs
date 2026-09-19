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
  Ctx (..),
  App,
  runApp,
) where

import Control.Foldl qualified as FL

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Char (toUpper)
import Data.IORef
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Version (showVersion)
import Initials
import PathUtils (isRelativeTo)
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

_au :: String
_au = "\x1f98b" -- Aglais urticae

tw :: String
tw = "\x2b51" -- Small star

tk :: String
tk = "\x2713" -- Tick

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
    <*> optional (optText "unified-name" 'u' [i|#{hi}#{hi} Base name for everything, except for the "Artist" tag|])
    <*> optional (optInt "album-num" 'b' "Add album number to destination")
    <*> optional (optText "artist" 'a' [i|#{hi} "Artist" tag|])
    <*> optional (optText "album" 'm' [i|#{hi} "Album" tag|])
    <*> argPath "src" "Source directory"
    <*> argPath "dst" "Destination directory"

-- | Utility description (help screen header).
description :: Description
description =
  [i|  Dahastes a.k.a. Procrustes SmArT is a CLI utility for copying subtrees containing
  supported audio files in sequence, naturally sorted. The end result is a flattened copy
  of the source subtree. "Flattened" means that only a namesake of the root source
  directory is created, where all the files get copied to, names prefixed with a serial
  number. Tag "Track Number" is set, tags "Title", "Artist", and "Album" can be replaced
  optionally. The writing process is strictly sequential: either starting with the number
  one file, or in the reversed order. This can be important for some mobile devices.
  #{hi} Really useful options. #{su} Suspicious media.
  v#{showVersion version}|]

data Ctx = Ctx
  { ctxSettings :: Settings
  }

type App = ReaderT Ctx IO

asksSettings :: (Settings -> a) -> App a
asksSettings entry = asks (entry . ctxSettings)

_settings :: App Settings
_settings = asks ctxSettings

runApp :: App ()
runApp = do
  copyAlbum

-- | Gets file size in bytes.
fsize :: FilePath -> IO Integer
fsize path = do
  status <- Posix.getFileStatus path
  return $ fromIntegral $ Posix.fileSize status

-- On Windows, use System.Directory.getFileSize

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
        size <- fsize path
        return (cnt + 1, total + size)
      else return (cnt, total)

-- | Serves the list of all audio files in the source directory.
_treeList :: Settings -> IO [FilePath]
_treeList args = do
  lst <- fold (lstree (sSrc args)) FL.list
  return $ filter (isAudioFile args) lst

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

-- | Makes a file name prefix or suffix out of the Artist Tag, if there is any.
artistGroomedToJoin :: Settings -> Bool -> String
artistGroomedToJoin args asPrefix
  | null name = name
  | asPrefix = name <> " - "
  | otherwise = " - " <> name -- asSuffix
 where
  name = maybe "" T.unpack (sArtistTag args)

-- | Makes destination file path.
shapeDst :: Settings -> FilePath -> Int -> Int -> FilePath -> FilePath -> FilePath
shapeDst args dstRoot totw n dstStep srcFile =
  let prefx =
        if sStripDecorations args && isNothing (sUnifiedName args)
          then ""
          else
            zeroPad n totw
              <> "-"
              <> if sPrependSubdirName args && length dstStep > 0
                then "[" <> concatMap (\c -> if c == '/' then "][" else [c]) dstStep <> "]-"
                else ""
      name = case sUnifiedName args of
        Just uName -> T.unpack uName <> artistGroomedToJoin args False
        Nothing -> baseName srcFile
      ext = case extension srcFile of
        Just extn -> "." <> extn
        Nothing -> ""
   in dstRoot </> (if sTreeDst args then dstStep else "") </> (prefx <> name <> ext)

-- | Makes one copy from source to destination directory.
copyFile :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> IO ()
copyFile args dstRoot total totw counter dstStep srcFile = do
  next <- counter 1
  let n = if sReverse args then total - next + 1 else next
  let dst = shapeDst args dstRoot totw n dstStep srcFile
  unless (sDryrun args) $ do
    cp srcFile dst
    setTagsToCopy args n dst
  putCopy args total totw n srcFile dst

-- | Walks the source tree, recreates source tree at destination.
traverseTreeDst :: FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> App ()
traverseTreeDst dstRoot total totw counter dstStep srcDir = do
  args <- asksSettings id
  (dirs, files) <- liftIO $ dirList args srcDir

  let walk dir = do
        let step = dstStep </> filename dir
        unless (sDryrun args) $ mkdir (dstRoot </> step)
        traverseTreeDst dstRoot total totw counter step dir

  mapM_ walk dirs
  mapM_ (liftIO . copyFile args dstRoot total totw counter dstStep) files

-- | Walks the source tree.
traverseFlatDst :: FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> App ()
traverseFlatDst dstRoot total totw counter dstStep srcDir = do
  args <- asksSettings id
  (dirs, files) <- liftIO $ dirList args srcDir

  let walk dir = do
        let step = dstStep </> filename dir
        traverseFlatDst dstRoot total totw counter step dir

  mapM_ walk dirs
  mapM_ (liftIO . copyFile args dstRoot total totw counter dstStep) files

-- | Walks the source tree backwards.
traverseFlatDstR :: FilePath -> Int -> Int -> Counter -> FilePath -> FilePath -> App ()
traverseFlatDstR dstRoot total totw counter dstStep srcDir = do
  args <- asksSettings id
  (dirs, files) <- liftIO $ dirList args srcDir

  let walk dir = do
        let step = dstStep </> filename dir
        traverseFlatDstR dstRoot total totw counter step dir

  mapM_ (liftIO . copyFile args dstRoot total totw counter dstStep) files
  mapM_ walk dirs

-- | Fires the files into the already existing destination directory.
traverseAlbum :: FilePath -> Int -> Int -> Counter -> Integer -> FilePath -> App ()
traverseAlbum execDst total totWidth counter byteCount src = do
  args <- asksSettings id

  liftIO $ putHeader args
  if sTreeDst args
    then traverseTreeDst execDst total totWidth counter "" src
    else
      if sReverse args
        then traverseFlatDstR execDst total totWidth counter "" src
        else traverseFlatDst execDst total totWidth counter "" src
  liftIO $ putFooter args total byteCount

-- | Copies the album.
copyAlbum :: App ()
copyAlbum = do
  args <- asksSettings id

  src <- realpath (sSrc args)

  unlessM (testdir src) $ do
    liftIO $ printf "Source directory \"%s\" does not exist\n" src
    exit (ExitFailure 1)

  (total, byteCount) <- liftIO $ treeCount args
  let totWidth = length $ show total

  when (total < 1) $ do
    liftIO $ printf "No audio files discovered in the source directory\n"
    exit ExitSuccess

  when (sCount args) $ do
    liftIO $ printf "Files: %d; Volume: %s\n" total (humanFine byteCount)
    exit ExitSuccess

  dst <- realpath (sDst args)

  unlessM (testdir dst) $ do
    liftIO $ printf "Destination directory \"%s\" does not exist\n" dst
    exit (ExitFailure 1)

  when (dst `isRelativeTo` src) $ do
    liftIO $ printf "Target directory \"%s\"\n" dst
    liftIO $ printf "is inside source \"%s\"\n" src
    exit (ExitFailure 1)

  -- The global (line) counter
  counter <- liftIO $ makeCounter
  -- exists from now on.
  --
  let srcName = basename src -- src must be a directory.
      albumNum = case sAlbumNum args of
        Just num -> zeroPad num 2 <> "-"
        Nothing -> ""
      baseDst = case sUnifiedName args of
        Just uname ->
          albumNum
            <> artistGroomedToJoin args True
            <> T.unpack uname
        Nothing -> albumNum <> srcName
      execDst = dst </> if sDropDst args then "" else baseDst

  if sDropDst args
    then traverseAlbum execDst total totWidth counter byteCount src
    else do
      exists <- testdir execDst
      if exists
        then
          if sOverwrite args
            then do
              unless (sDryrun args) $ do
                rmtree execDst
                mkdir execDst
              traverseAlbum execDst total totWidth counter byteCount src
            else
              liftIO $ printf "Destination directory \"%s\" already exists\n" execDst
        else do
          unless (sDryrun args) $ mkdir execDst
          traverseAlbum execDst total totWidth counter byteCount src

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
          (unitName, numDecimals, _, _) = unitList !! unitIdx
       in printf ("%." ++ show numDecimals ++ "f%s") quotient unitName
  | bytes == 0 = "0"
  | bytes == 1 = "1"
  | otherwise = "humanFine error; bytes: " ++ show bytes
 where
  unitList :: [(String, Int, String, String)]
  unitList =
    [ ("", 0, "1024^0", "Byte")
    , ("kB", 0, "1024^1", "Kilobyte")
    , ("MB", 1, "1024^2", "Megabyte")
    , ("GB", 2, "1024^3", "Gigabyte")
    , ("TB", 2, "1024^4", "Terabyte")
    , ("PB", 2, "1024^5", "Petabyte")
    , ("EB", 2, "1024^6", "Exabyte")
    , ("ZB", 2, "1024^7", "Zettabyte")
    , ("YB", 2, "1024^8", "Yottabyte")
    ]

  integerLogBase :: Integer -> Integer -> Int
  integerLogBase b n
    | n < b = 0
    | otherwise = 1 + integerLogBase b (n `div` b)

-- | Prints the header of the output to the console.
putHeader :: Settings -> IO ()
putHeader args = do
  if sVerbose args || sDryrun args
    then putStr ""
    else putStr "Start "

-- | Prints a single file copy info to the console.
putCopy :: Settings -> Int -> Int -> Int -> FilePath -> FilePath -> IO ()
putCopy args total totw n srcFile dstFile = do
  if sVerbose args || sDryrun args
    then do
      size <- fsize srcFile
      let fmt =
            "%"
              <> printf "%d" totw
              <> [i|d#{if sDryrun args then tw else tw}%d %s|]
              <> (if sDryrun args then [i| #{tk} #{humanFine size}|] else "")
              <> "\n"
       in putStr (printf fmt n total dstFile)
    else putStr "."

-- | Prints the footer of the output to the console.
putFooter :: Settings -> Int -> Integer -> IO ()
putFooter args total byteCount = do
  let bcount = humanFine byteCount
  if sVerbose args || sDryrun args
    then
      if sDryrun args
        then putStr (printf "Total of %d file(s) good to copy; Volume: %s\n" total bcount)
        else putStr (printf "Total of %d file(s) copied; Volume: %s\n" total bcount)
    else putStr (printf " Done(%d); Volume: %s\n" total bcount)
