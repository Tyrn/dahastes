{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import qualified Control.Foldl as FL
import Control.Monad.Extra
import Data.List (sortBy)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Lib
import System.IO hiding (FilePath, stderr, stdout)
import Turtle hiding (find, printf, sortBy, stderr, stdout)
import Prelude hiding (FilePath)

-- | Serves the list of all audio files in the source directory.
listTree :: Settings -> IO [FilePath]
listTree args = do
  lst <- fold (lstree (sSrc args)) FL.list
  return $ filter (isAudioFile args) lst

-- Builds compare function according to options (for listDir only)
makeCompare :: Settings -> (FilePath -> FilePath -> Ordering)
makeCompare args =
  let path = strp . dropExtension
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
listDir :: Settings -> FilePath -> IO ([FilePath], [FilePath])
listDir args src = do
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
        Nothing -> strp $ baseName srcFile
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
  (dirs, files) <- listDir args srcDir

  let walk dir = do
        let step = dstStep </> filename dir -- dir has NO trailing slash!
        mkdir (dstRoot </> step)
        traverseTreeDst args dstRoot total totw counter step dir

  mapM_ walk dirs
  mapM_ (copyFile args dstRoot total totw counter dstStep) files

-- | Walks the source tree.
traverseFlatDst :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> IO ()
traverseFlatDst args dstRoot total totw counter srcDir = do
  (dirs, files) <- listDir args srcDir
  mapM_ (traverseFlatDst args dstRoot total totw counter) dirs
  mapM_ (copyFile args dstRoot total totw counter (wrap "")) files

-- | Walks the source tree backwards.
traverseFlatDstR :: Settings -> FilePath -> Int -> Int -> Counter -> FilePath -> IO ()
traverseFlatDstR args dstRoot total totw counter srcDir = do
  (dirs, files) <- listDir args srcDir
  mapM_ (copyFile args dstRoot total totw counter (wrap "")) files
  mapM_ (traverseFlatDstR args dstRoot total totw counter) dirs

-- | Copies the album.
copyAlbum :: Settings -> IO ()
copyAlbum args = do
  checkTree <- listTree args

  dst <- realpath (sDst args)
  let total = length checkTree
  let totWidth = length $ show total
  counter <- makeCounter
  src <- realpath (sSrc args)

  let srcName = dirname src -- src HAS a trailing slash!
  let albumNum = case sAlbumNum args of
        Just num -> zeroPad num 2 <> "-"
        Nothing -> ""
  let baseDst = case sUnifiedName args of
        Just uname ->
          wrap $
            albumNum
              <> artistPrefix args
              <> " - "
              <> T.unpack uname
        Nothing -> wrap $ albumNum <> strp srcName
  let execDst = dst </> if sDropDst args then wrap "" else baseDst

  if sDropDst args
    then return ()
    else mkdir execDst

  putHeader args
  if sTreeDst args
    then traverseTreeDst args execDst total totWidth counter (wrap "") src
    else
      if sReverse args
        then traverseFlatDstR args execDst total totWidth counter src
        else traverseFlatDst args execDst total totWidth counter src
  putFooter args total

main :: IO ()
main = do
  args <- options description settingsP
  copyAlbum args
