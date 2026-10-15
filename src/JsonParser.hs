{-
-- EPITECH PROJECT, 2024
-- Parser.hs
-- File description:
-- mypandoc
-}

module JsonParser
    ( parseJsonValue,
      JsonValue(JsonNull, JsonBool, JsonNumber, JsonString, JsonArray, JsonObject),
    ) where

import Parser
import Control.Applicative

data JsonValue = JsonNull | JsonBool Bool | JsonNumber Int | JsonString String | JsonArray [JsonValue] | JsonObject [(String, JsonValue)] deriving (Show)

parseJsonNull :: Parser JsonValue
parseJsonNull = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser (parseWord "null") rest of
            Right (_, rest1) -> Right (JsonNull, rest1)
            Left _ -> Left "Fail"
        Left _  -> Left "Fail"

parseJsonBool :: Parser JsonValue
parseJsonBool = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser (parseWord "true") rest of
            Right (_, rest1) -> Right (JsonBool True, rest1)
            _ -> case runParser (parseWord "false") rest of
                Right (_, rest1) -> Right (JsonBool False, rest1)
                Left _ -> Left "Fail"
        Left _  -> Left "Fail"

parseJsonNum :: Parser JsonValue
parseJsonNum = Parser fc where
    fc str = case runParser (parseMany (parseChar ' ')) str of
        Right (_, rest) -> case runParser parseUInt rest of
            Right (num, rest1) -> Right (JsonNumber num, rest1)
            Left _ -> Left "Fail"
        Left _  -> Left "Fail"

parseJsonString :: Parser JsonValue
parseJsonString = Parser fc where
    fc str = case runParser parseString str of
        Right (str, rest1) -> Right (JsonString str, rest1)
        Left _ -> Left "Fail"

parseJsonValue :: Parser JsonValue
parseJsonValue = parseJsonBool <|> parseJsonNull <|> parseJsonNum
    <|> parseJsonString <|> parseJsonArray <|> parseJsonObject

parseJsonArray :: Parser JsonValue
parseJsonArray = parseChar '[' >> parseMany (parseAnyChar " \t\n") >>
    ((:) <$> parseJsonValue <*>
    parseMany (parseChar ',' *>
    parseMany (parseAnyChar " \t\n") *> parseJsonValue)
    <|> pure []) >>= \values ->
    parseMany (parseAnyChar " \t\n")
    >> parseChar ']' >> return (JsonArray values)

parseJsonKeyValue :: Parser (String, JsonValue)
parseJsonKeyValue = do
    key <- parseString
    _ <- parseMany (parseChar ':' *> parseAnyChar " \t\n")
    val <- parseJsonValue
    _ <- parseMany (parseChar ',')
    return (key, val)

parseJsonObject :: Parser JsonValue
parseJsonObject = parseChar '{' >> parseMany (parseAnyChar " \t\n") >>
    ((:) <$> parseJsonKeyValue <*>
    parseMany (parseAnyChar " \t\n" *> parseJsonKeyValue)
    <|> pure []) >>= \values ->
    parseMany (parseAnyChar " \t\n") >> parseChar '}'
    >> return (JsonObject values)
