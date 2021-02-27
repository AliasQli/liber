{-# LANGUAGE TemplateHaskell #-}

module Main where

import Display
import Parser.ToTemplate

script = $(embedScript "script.scr")

main = display script
