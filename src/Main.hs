module Main where

import           Utils                          ( decode )
import           Person                         ( Person )
import           FromJValue                     ( fromJValue )
import           JValueParser                   ( parse
                                                , parseJson
                                                )

main :: IO ()
main = do
  input <- readFile "./src/person.json"
  case decode $ parse parseJson input of
    Left  err                -> fail err
    Right (person :: Person) -> print person
