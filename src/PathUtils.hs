module PathUtils (isRelativeTo, relativeSuffix) where

import Data.List (isPrefixOf, stripPrefix)
import System.FilePath

-- | Split a path into components, normalised and with "." removed.
pathComponents :: FilePath -> [FilePath]
pathComponents = filter (/= ".") . splitDirectories . normalise

-- | Check whether the first path is relative to the second path.
isRelativeTo :: FilePath -> FilePath -> Bool
isRelativeTo child parent =
   pathComponents parent `isPrefixOf` pathComponents child

-- | Return the suffix of child after parent, if child is relative to parent.
relativeSuffix :: FilePath -> FilePath -> Maybe FilePath
relativeSuffix child parent =
   case stripPrefix (pathComponents parent) (pathComponents child) of
      Just [] -> Just "."
      Just rest -> Just (joinPath rest)
      Nothing -> Nothing
