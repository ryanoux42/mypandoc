{-
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- XMLParser
-}

module XMLParser
    ( XMLValue(XMLElement, XMLText),
      parseXMLValue
    ) where

import Parser
import Control.Applicative

data XMLValue = XMLElement(String, [(String, String)], [XMLValue]) | XMLText String deriving (Show)

parseXMLText :: Parser XMLValue
parseXMLText = XMLText <$> parseSome (parseNotTheseChars "\n<")

parseXMLAttribute :: Parser (String, String)
parseXMLAttribute = do
    parseMany (parseAnyChar " \t\n")
    attributeName <- parseMany (parseNotTheseChars "= \t\n")
    parseMany (parseAnyChar " \t")
    parseChar '='
    parseMany (parseAnyChar " \t")
    parseChar '"'
    attributeValue <- parseMany (parseNotThisChar '"')
    parseChar '"'
    return (attributeName, attributeValue)

parseXMLElement :: Parser XMLValue
parseXMLElement = do
    parseMany (parseAnyChar " \t\n")
    parseChar '<'
    tag <- parseMany (parseNotTheseChars " >")
    attrs <- parseMany parseXMLAttribute
    parseChar '>'
    children <- parseMany parseChild
    parseMany (parseAnyChar " \t\n")
    parseWord ("</" ++ tag ++ ">")
    return (XMLElement (tag, attrs, children))

parseChild :: Parser XMLValue
parseChild = parseXMLElement <|> parseXMLText

parseXMLValue :: Parser XMLValue
parseXMLValue = parseXMLElement
