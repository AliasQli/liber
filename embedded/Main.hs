{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed
import Display

utf8Script = $(embedFile "script.scr")

main = display utf8Script
