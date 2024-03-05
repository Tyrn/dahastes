{-# LANGUAGE OverloadedStrings #-}


-- | Support for Procrustes SmArT utility (audio album builder).
module Lib
    ( strp
    , wrap
    , baseName
    , makeCounter
    , Counter
    , isAudioFile
    , zeroPad
    , strStripNumbers
    , cmpstrNaturally
    , makeInitials
    , setTagsToCopy
    , Settings(..)
    , description
    , settingsP
    , putHeader
    , putCopy
    , putFooter
    ) where

import Turtle hiding (printf, stdout, stderr, find)
import Prelude hiding (FilePath)
import System.IO hiding (FilePath, stdout, stderr)
import Data.List.Split
import Data.IORef
import qualified Data.Text as T
import Text.Printf
import Text.Regex.TDFA
import Data.List hiding (find)
import Data.Monoid
import Sound.HTagLib
import Data.Maybe
import qualified Filesystem.Path.CurrentOS as FPS
import Data.Either.Extra

{- Command line parser -}


-- | Represents command line options.
data Settings = Settings
  { sVerbose           :: Bool
  , sDropTracknumber   :: Bool
  , sStripDecorations  :: Bool
  , sFileTitle         :: Bool
  , sFileTitleNum      :: Bool
  , sSortLex           :: Bool
  , sTreeDst           :: Bool
  , sDropDst           :: Bool
  , sReverse           :: Bool
  , sFileType          :: Maybe Text
  , sPrependSubdirName :: Bool
  , sUnifiedName       :: Maybe Text
  , sAlbumNum          :: Maybe Int
  , sArtistTag         :: Maybe Text
  , sAlbumTag          :: Maybe Text
  , sSrc               :: FilePath
  , sDst               :: FilePath
  }


-- | Command line options definition.
settingsP :: Parser Settings
settingsP =
  Settings <$> switch "verbose" 'v' "Unless verbose, just progress bar is shown"
           <*> switch "droptracknumber" 'd' "Do not set track numbers"
           <*> switch "stripdecorations" 's' "Strip file and directory name decorations"
           <*> switch "filetitle" 'f' "Use file name for title tag"
           <*> switch "filetitlenum" 'F' "Use numbered file name for title tag"
           <*> switch "sortlex" 'x' "Sort files lexicographically"
           <*> switch "treedst" 't' "Retain the tree structure of the source album at destination"
           <*> switch "dropdst" 'p' "Do not create destination directory"
           <*> switch "rev" 'r' "Copy files in reverse order (number one file is the last to be copied)"
           <*> optional (optText "filetype" 'e' "Accept only audio files of the specified type")
           <*> switch "prependsubdirname" 'i' "prepend current subdirectory name to a file name"
           <*> optional (optText "unifiedname" 'u' "Base name for everything, except for the \"Artist\" tag")
           <*> optional (optInt "albumnum" 'b' "Add album number to destination")
           <*> optional (optText "artisttag" 'a' "\"Artist\" tag")
           <*> optional (optText "albumtag" 'g' "\"Album\" tag")
           <*> argPath "src" "Source directory"
           <*> argPath "dst" "Destination directory"


-- | Utility description (help screen header).
description :: Description
description =
  "pch \"Procrustes\" SmArT is a CLI utility for copying subtrees containing supported\n\
  \audio files in sequence, naturally sorted.\n\
  \The end result is a flattened copy of the source subtree. \"Flattened\" means\n\
  \that only a namesake of the root source directory is created, where all the files get\n\
  \copied to, names prefixed with a serial number. Tag \"Track Number\"\n\
  \is set, tags \"Title\", \"Artist\", and \"Album\" can be replaced optionally.\n\
  \The writing process is strictly sequential: either starting with the number one file,\n\
  \or in the reversed order. This can be important for some mobile devices."
  

{- Counter, mostly global -}


-- | Represents a nonlocal counter.
type Counter = Int -> IO Int


-- | Returns a function capable of returning increasing values (counter).
makeCounter :: IO Counter
makeCounter = do
  r <- newIORef 0
  return (\i -> do modifyIORef r (+i)
                   readIORef r)


{- Audio tags management -}


-- | Makes custom title tag
shapeTitle :: Settings -> Int -> String -> String -> Text
shapeTitle args n fileName s =
  T.pack (if (sFileTitleNum args)
            then (printf "%d>%s" n fileName) -- Add Track Number to Title
            else if (sFileTitle args)
                   then fileName
                   else (printf "%d %s" n s))


-- | Sets tags to the destination file.
setTagsToCopy :: Settings -> Int -> Int -> FilePath -> IO ()
setTagsToCopy args total trackNum file
  | (sArtistTag args) /= Nothing && isAlbumTag =
                 st $  titleSetter  (mkTitle $ tt ((makeInitials $ T.unpack artist)
                                           ++ " - " ++ T.unpack album))
                    <> artistSetter (mkArtist artist)
                    <> albumSetter  (mkAlbum album)
                    <> track

  | (sArtistTag args) /= Nothing =
                 st $  titleSetter  (mkTitle $ tt $ T.unpack artist)
                    <> artistSetter (mkArtist artist)
                    <> track

  | isAlbumTag = st $  titleSetter  (mkTitle $ tt $ T.unpack album)
                    <> albumSetter  (mkAlbum album)
                    <> track

  | otherwise = return ()

  where st         = setTags (strp file) Nothing
        tt         = shapeTitle args trackNum (strp $ baseName file)
        artist     = fromMaybe "*" (sArtistTag args)
        album      = case (sUnifiedName args) of
                       Just uname -> uname
                       Nothing    -> fromMaybe "*" (sAlbumTag args)
        isAlbumTag = (sAlbumTag args) /= Nothing || (sUnifiedName args) /= Nothing
        track      = if (sDropTracknumber args)
                       then mempty
                       else trackNumberSetter (mkTrackNumber trackNum)


{- FilePath helpers -}


-- | Extracts String From FilePath
-- (good until deprecated system-filepath removed).
strp :: FilePath -> String
strp path = T.unpack $ fromRight "" (FPS.toText path)


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
  let  ext = case extension file of
               Just ext -> (T.unpack $ T.toUpper ext)
               Nothing  -> ""
  in   elem ext checkList
  where checkList = case (sFileType args) of
                      Just ftype -> [dropWhile (=='.') (T.unpack $ T.toUpper ftype)]
                      Nothing    -> ["MP3", "M4A", "M4B", "OGG", "WMA", "FLAC"]


-- | Returns a zero-padded numeric literal.
--
-- Examples:
--
-- >>> zeroPad 3 5
-- "00003"
-- >>> zeroPad 15331 3
-- "15331"
zeroPad :: Int -> Int -> String
zeroPad n len = printf ("%0" ++ (printf "%d" len) ++ "d") n


-- | Returns a list of integer numbers embedded in a string arguments.
--
-- Examples:
--
-- >>> strStripNumbers "ab11cdd2k.144"
-- [11,2,144]
-- >>> strStripNumbers "Ignacio Vazquez-Abrams"
-- []
strStripNumbers :: String -> [Int]
strStripNumbers str =
  let numbers = concat (str =~ ("[0-9]+" :: String) :: [[String]])
  in  [read i :: Int | i <- numbers]


-- | If both strings contain digits, returns numerical comparison based on the numeric
-- values embedded in the strings, otherwise returns the standard string comparison.
-- The idea of the natural sort as opposed to the standard lexicographic sort is one of coping
-- with the possible absence of the leading zeros in 'numbers' of files or directories.
--
-- Examples:
--
-- >>> cmpstrNaturally "" ""
-- EQ
-- >>> cmpstrNaturally "2a" "10a"
-- LT
-- >>> cmpstrNaturally "alfa" "bravo"
-- LT
cmpstrNaturally :: String -> String -> Ordering
cmpstrNaturally x y =
  let nx = strStripNumbers x
      ny = strStripNumbers y
  in  if nx /= [] && ny /=  []
        then compare nx ny
        else compare x y


-- | Removes all double quoted substrings, if any, from a string.
--
-- Examples:
--
-- >>> removeQuotedSubstrings "alfa"
-- "alfa"
-- >>> removeQuotedSubstrings "\"\"ngoro\"dup\"lai \"ming\""
-- " ngoro lai  "
removeQuotedSubstrings :: String -> String
removeQuotedSubstrings str =
  let quoteds  = filter (\s -> length s > 0 && head s == '"')
               $ concat (str =~ ("\"(\\.|[^\"\\])*\"" :: String) :: [[String]])
  in  T.unpack $ foldr (\quoted acc -> T.replace (T.pack quoted) " " acc)
                       (T.pack str) quoteds
  

-- | Reduces a string of names to initials.
--
-- Examples:
--
-- >>> makeInitials " "
-- "."
-- >>> makeInitials "John ronald reuel\tTolkien"
-- "J.R.R.T."
-- >>> makeInitials "e. B. Sledge"
-- "E.B.S."
-- >>> makeInitials "Apsley  Cherry-Garrard "
-- "A.C-G."
-- >>> makeInitials "Windsor Saxe-\tCoburg - Gotha"
-- "W.S-C-G."
-- >>> makeInitials "Elisabeth Kubler-- - Ross"
-- "E.K---R."
-- >>> makeInitials "Fitz-Simmons Ashton-Burke Leigh"
-- "F-S.A-B.L."
-- >>> makeInitials "Arleigh\"31-knot\"Burke  "
-- "A.B."
makeInitials :: String -> String
makeInitials grandName =
  let parts = splitOn "-" $ removeQuotedSubstrings grandName
      splitPart = \part -> concat (part =~ ("[^ \t]+" :: String) :: [[String]])
      inits = (\part -> intercalate "." [[head w] | w <- splitPart part]) <$> parts
  in  T.unpack $ T.toUpper $ fromString $ intercalate "-" inits ++ "."


{- Console output -}


-- | Prints the header of the output to the console.
putHeader :: Settings -> IO ()
putHeader args = do
  if (sVerbose args)
    then putStr ""
    else putStr "Start "


-- | Prints a single file copy info to the console.
putCopy :: Settings -> Int -> Int -> Int -> FilePath -> IO ()
putCopy args total totw n dstFile = do
  if (sVerbose args)
    then let fmt = "%" ++ (printf "%d" totw) ++ "d\x2698%d %s\n"
         in  putStr (printf fmt n total (strp dstFile))
    else putStr "."


-- | Prints the footer of the output to the console.
putFooter :: Settings -> Int -> IO ()
putFooter args total = do
  if (sVerbose args)
    then putStr (printf "Total of %d file(s) copied\n" total)
    else putStr (printf " Done(%d)\n" total)
