{-# LANGUAGE TemplateHaskell #-}

module Main where

import Display
import Parser.Type

script = $(embedScript "script.scr")

main = display script
