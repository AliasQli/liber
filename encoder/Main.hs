module Main where
import           MegaParser.Parse
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let arg0:arg1:_ = zipWith (\a b -> if null a then b else a) (args ++ repeat "") ["script.txt", "script.scr"]
  encodeScript arg0 arg1
  putStrLn "Succeeded."
