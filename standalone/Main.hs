{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString as B
import Display
import Path
import System.Environment
import System.FilePath

main = do
  args <- getArgs
  filepath <-
    if
        | null args ->
          concatRelativePath "script.scr"
        | path : _ <- args ->
          if isRelative path
            then concatRelativePath path
            else return path
        | otherwise -> error "Invalid argument."
  utf8Script <- B.readFile filepath
  let script = getScript utf8Script
  display script
