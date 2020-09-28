module JValueParser  where

import           Data.Map.Strict               as M (Map, fromList)
import           JValue                        (JValue (..))

import           Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (ParseError, Parser)

parse :: Parser a -> String -> Either ParseError a
parse p = P.parse p []

parseJson :: Parser JValue
parseJson = parseObjectOrArray

parseObjectOrArray :: Parser JValue
parseObjectOrArray = JObject <$> parseObject <|> JArray <$> parseArray

parseObject :: Parser (Map String JValue)
parseObject = M.fromList <$> parseCommaSepList '{' '}' parseKeyValue

parseArray :: Parser [JValue]
parseArray = parseCommaSepList '[' ']' parseValue

parseCommaSepList :: Char -> Char -> Parser a -> Parser [a]
parseCommaSepList start end p =
    trim (char start) *>
    sepBy p (char ',' <* spaces)
    <* trim (char end)

parseKeyValue :: Parser (String, JValue)
parseKeyValue = do
    key <- parseString
    _ <- trim (char ':')
    val <- parseValue
    pure (key, val)

parseValue :: Parser JValue
parseValue = parseObjectOrArray
                <|> JString <$> parseString
                <|> JBool <$> parseBool
                <|> JNum <$> parseNum
                <|> JNull <$ string "null"

parseString :: Parser String
parseString = char '"' *> manyTill anyChar (char '"')

parseBool :: Parser Bool
parseBool = True <$ string "true" <|> False <$ string "false"

parseNum :: Parser Double
parseNum = read <$> many digit

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces
