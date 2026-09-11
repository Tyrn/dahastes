module PathUtils (
   isRelativeTo,
   relativeSuffix,
) where

import Data.List (isPrefixOf, stripPrefix)
import System.FilePath

{- | Check whether the first path is relative to the second path.

>>> isRelativeTo "foo/bar/baz" "foo/bar"
True
>>> isRelativeTo "foo/bar" "foo/bar/baz"
False
>>> isRelativeTo "foo/bar" "foo/bar"
True
>>> isRelativeTo "/a/b" "/a"
True
>>> isRelativeTo "/a" "/"
True
-}
isRelativeTo :: FilePath -> FilePath -> Bool
isRelativeTo child parent =
   let childParts = splitDirectories (normalise child)
       parentParts = splitDirectories (normalise parent)
    in parentParts `isPrefixOf` childParts

-- | Return the suffix of child after parent, if child is relative to parent.
relativeSuffix :: FilePath -> FilePath -> Maybe FilePath
relativeSuffix child parent =
   let childParts = splitDirectories (normalise child)
       parentParts = splitDirectories (normalise parent)
    in case stripPrefix parentParts childParts of
         Just rest -> Just (joinPath rest)
         Nothing -> Nothing
