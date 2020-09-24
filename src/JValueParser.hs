module JValueParser  where

import           JValue                        (JValue (..))

import           Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (ParseError, Parser)

parse :: Parser a -> String -> Either ParseError a
parse p = P.parse p []

parseJson :: Parser JValue
parseJson = undefined
