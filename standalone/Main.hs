{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString as B
import Display
import System.Environment
import System.FilePath

main = do
  args <- getArgs
  filepath <-
    if
        | null args ->
          flip replaceFileName "script.scr" <$> getExecutablePath
        | path : _ <- args ->
          if isRelative path
            then flip replaceFileName path <$> getExecutablePath
            else return path
        | otherwise -> error "Invalid argument."
  utf8Script <- B.readFile filepath
  let script = getScript utf8Script
  display script
