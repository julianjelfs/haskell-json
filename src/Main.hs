module Main where

import           JValueParser (parse, parseJson)

main :: IO ()
main = do
  input <- readFile ("./src/testdata.json")
  case parse parseJson input of
    Left err   -> fail (show err)
    Right json -> putStrLn (show json)
