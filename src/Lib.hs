{-
-- EPITECH PROJECT, 2024
-- Lib.hs
-- File description:
-- mypandoc
-}

module Lib
    ( pandoc
    ) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Applicative ( Alternative((<|>), empty) )
import Parser
import JsonParser
import PrintValues
import System.Environment
import System.IO
import XMLParser

data Options = Options {
    ifile :: String,
    oformat :: String,
    ofile :: String,
    iformat :: String
    } deriving (Show)

defaultOpt :: Options
defaultOpt = Options
    { ifile = ""
    , oformat = ""
    , ofile = ""
    , iformat = ""
    }

catchError :: String -> IO a
catchError errMsg =
    putStrLn ("Error: " ++ errMsg) >> printUsage >> exitWith (ExitFailure 84)

printUsage :: IO ()
printUsage =
    putStrLn "Usage: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]" >>
    putStrLn "\n\tifile\tpath to the file to convert" >>
    putStrLn "\toformat\toutput format (xml, json, markdown)" >>
    putStrLn "\tofile\tpath to the output file" >>
    putStrLn "\tiformat\tinput format (xml, json, markdown)"

getOpts :: Options -> [String] -> Either String Options
getOpts current [] = Right current
getOpts current ("-i":val:rest) = getOpts (current {ifile = val}) rest
getOpts current ("-f":val:rest) = getOpts (current {oformat = val}) rest
getOpts current ("-o":val:rest) = getOpts (current {ofile = val}) rest
getOpts current ("-e":val:rest) = getOpts (current {iformat = val}) rest
getOpts _ _ = Left "Invalid arguments"

printPandoc :: String -> String -> IO ()
printPandoc "" res = putStrLn res
printPandoc filepath res = do
    handle <- openFile filepath WriteMode
    hPutStr handle res
    hClose handle

choicePrintPandoc :: Options -> JsonValue -> IO ()
choicePrintPandoc opt res
    | (oformat opt) == "json" = printPandoc (ofile opt) (printJson res)
    | (oformat opt) == "xml" = printPandoc (ofile opt) (printXMLbalise res)
    | (oformat opt) == "markdown" = printPandoc (ofile opt) (printMarkFile res)
    | otherwise = catchError "Unknow format output"

attrToJson :: (String, String) -> (String, JsonValue)
attrToJson (key, value) = (key, JsonString value)

-- xmlToJsonObject :: XMLValue -> (String, JsonValue)
-- xmlToJsonObject (XMLText _) = ("", JsonString "")
-- xmlToJsonObject (XMLElement (name, attrs, children)) = (name, JsonObject (map xmlToJsonObject children))

-- recusiveXmlToJson :: [XMLValue] -> [(String, JsonValue)]
-- recusiveXmlToJson [] = []
-- recusiveXmlToJson XMLText x =
-- recusiveXmlToJson ((XMLElement (name, attrs, children)):xs) = ((name, JsonObject (map xmlToJsonObject children))) : recusiveXmlToJson xs

tabXmlToJson :: [XMLValue] -> [JsonValue]
tabXmlToJson [] = []
tabXmlToJson (x:xs) = xmlToJson x : tabXmlToJson xs

xmlToJson :: XMLValue -> JsonValue
xmlToJson (XMLText text) = JsonString text
xmlToJson (XMLElement ((_:_), [], [(XMLText x)])) = JsonString x
xmlToJson (XMLElement (_, _, (XMLElement (header, headerAttribute, _):
    XMLElement (body, _, bodyChildren):_))) =
    JsonObject ((header, JsonObject (map attrToJson headerAttribute))
    : [(body, JsonArray (tabXmlToJson bodyChildren))])

checkAllCasePandoc :: Options -> String -> IO ()
checkAllCasePandoc opt content = case runParser parseJsonValue content of
                Right (res, _) -> choicePrintPandoc opt res
                _ -> case runParser parseXMLValue content of
                    Right (res1, _) -> choicePrintPandoc opt (xmlToJson res1)
                    _ -> putStrLn "Wrong parsing xml json"

activePandoc :: Options -> String -> IO ()
activePandoc opt content
    | (iformat opt) == "" = checkAllCasePandoc opt content
    | (iformat opt) == "xml" = case runParser parseXMLValue content of
                Right (res, _) -> choicePrintPandoc opt (xmlToJson res)
                _ -> putStrLn "Wrong parsing xml"
    | (iformat opt) == "json" =  case runParser parseJsonValue content of
        Right (res, _) -> choicePrintPandoc opt res
        _ -> putStrLn "Wrong parsing json"
    | otherwise = catchError "Unknow format input"

pandoc :: IO ()
pandoc = do
    tempArgs <- getArgs
    let len = length tempArgs
    if len >= 4 && mod len 2 == 0
    then case getOpts defaultOpt tempArgs of
        Right opts -> do
            content <- readFile (ifile opts)
            activePandoc opts content
        Left err -> catchError err
    else catchError "Invalid number of arguments"