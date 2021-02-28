module Path where

import System.Environment (getExecutablePath)
import System.FilePath (replaceFileName)

concatRelativePath path = flip replaceFileName path <$> getExecutablePath