module Path where

import           System.Environment (getExecutablePath)
import           System.FilePath

concatRelativePath :: String -> IO FilePath
concatRelativePath path = replaceFileName <$> getExecutablePath <*> pure path

relativeToSelf :: FilePath -> IO FilePath
relativeToSelf path = if isRelative path
  then replaceFileName <$> getExecutablePath <*> pure path
  else pure path
