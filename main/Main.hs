module Main where

import           Data.Binary        (decodeFile)
import           Display.Display            (display)
import           Path               (relativeToSelf)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let rawPath = if null args then "script.scr" else head args
  path <- relativeToSelf rawPath
  scr <- decodeFile path
  print scr
  putStrLn "----------"
  display scr
