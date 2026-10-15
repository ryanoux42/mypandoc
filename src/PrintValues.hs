{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-camil.kaddouri
-- File description:
-- PrintValues
-}

module PrintValues
    ( printJson,
      printXMLbalise,
      printMarkFile
    ) where
import JsonParser
import XMLParser

printJsonArray :: [JsonValue] -> String
printJsonArray [] = ""
printJsonArray (x:xs) = ", " ++ printJson x ++ printJsonArray xs

printJsonObject :: [(String, JsonValue)] -> String
printJsonObject [] = ""
printJsonObject ((x,y):xs) = ",\"" ++ x ++ "\": "
    ++ printJson y ++ printJsonObject xs

printJson :: JsonValue -> String
printJson JsonNull = "null"
printJson (JsonBool x) = if x then "true" else "false"
printJson (JsonNumber x) = show x
printJson (JsonString x) = x
printJson (JsonArray []) = "[]"
printJson (JsonArray (x:xs)) = "[" ++ printJson x ++ printJsonArray xs ++ "]"
printJson (JsonObject []) = "{}"
printJson (JsonObject ((x, y):xs)) = "{\"" ++ x ++ "\": "
    ++ printJson y ++ printJsonObject xs ++ "}"

printXMLArray :: [JsonValue] -> String
printXMLArray [] = ""
printXMLArray (x:xs) = "<value>" ++ printXML x
    ++ "</value>" ++ printXMLArray xs

printXMLObject :: [(String, JsonValue)] -> String
printXMLObject [] = ""
printXMLObject ((x,y):xs) = "<" ++ x ++ ">" ++ printXML y
    ++ "</" ++ x ++ ">" ++ printXMLObject xs

printXMLList :: JsonValue -> String
printXMLList (JsonArray (x:xs)) = "<array>" ++ "<value>" ++ printXML x
    ++ "</value>" ++ printXMLArray xs ++ "</array>"

printXML :: JsonValue -> String
printXML JsonNull = ""
printXML (JsonBool x) = if x then "true" else "false"
printXML (JsonNumber x) = show x
printXML (JsonString x) = x
printXML (JsonArray []) = ""
printXML (JsonArray x)
    | length x == 1 = "<paragraph>" ++ printXML (head x) ++ "</paragraph>"
    | otherwise = printXMLList (JsonArray x)
printXML (JsonObject []) = ""
printXML (JsonObject ((x, y):xs)) = "<" ++ x ++ ">"
    ++ printXML y ++ "</" ++ x ++ ">" ++ printXMLObject xs

printBodyXML :: [(String, JsonValue)] -> String
printBodyXML [] = "<body></body>"
printBodyXML ((x, y):xs) = "<" ++ x ++ ">" ++ printXML y
    ++ "</" ++ x ++ ">" ++ printXMLObject xs

printXMLheaderObject :: [(String, JsonValue)] -> String
printXMLheaderObject [] = ""
printXMLheaderObject ((x, y):xs) = " " ++ x ++ "=\"" ++ printXML y
    ++ "\"" ++ printXMLheaderObject xs

printHeaderXML :: String -> JsonValue -> String
printHeaderXML x (JsonObject y) = "<" ++ x
    ++ printXMLheaderObject y ++ "></" ++ x ++ ">"

printXMLbalise :: JsonValue -> String
printXMLbalise (JsonObject ((x, y):xs)) = "<document>"
    ++ printHeaderXML x y ++ printBodyXML xs ++ "</document>"

printSpace :: Int -> String
printSpace 0 = ""
printSpace nb = " " ++ printSpace (nb - 1)

printMarkArray :: [JsonValue] -> Int -> String
printMarkArray [] _ = ""
printMarkArray (x:xs) nb = " " ++ printMark x nb ++ printMarkArray xs nb

printMarkList :: [JsonValue] -> String
printMarkList [] = ""
printMarkList (x:xs) = "\n+" ++ printMark x 0 ++ printMarkList xs

printMarkObject :: [(String, JsonValue)] -> Int -> String
printMarkObject [] _ = ""
printMarkObject ((_,y):xs) nb = "\n" ++ printMark y nb ++ printMarkObject xs nb

printMark' :: JsonValue -> Int -> String
printMark' (JsonObject []) _ = ""
printMark' (JsonObject (("bold",y):xs)) _ = "**" ++ printMark y 0
    ++ "**" ++ printMarkObject xs 0
printMark' (JsonObject (("italic",y):xs)) _ = "*" ++ printMark y 0
    ++ "*" ++ printMarkObject xs 0
printMark' (JsonObject (("code",y):xs)) _ = "`" ++ printMark y 0
    ++ "`" ++ printMarkObject xs 0
printMark' (JsonObject (("codeblock",y):xs)) _ = "```" ++ printMark y 0
    ++ "```" ++ printMarkObject xs 0 ++ "\n"
printMark' (JsonObject (("title",y):xs)) _ = "\n# " ++ printMark y 0
    ++ "\n" ++ printMarkObject xs 0
printMark' (JsonObject (("list",JsonArray y):xs)) _ =  printMarkList y ++ "\n"
    ++ printMarkObject xs 0
printMark' (JsonObject ((_,y):xs)) _ = "\n"
    ++ printMark y 0 ++ printMarkObject xs 0

printMark :: JsonValue -> Int -> String
printMark JsonNull _ = ""
printMark (JsonBool x) _ = if x then "TRUE" else "FALSE"
printMark (JsonNumber x) _ = show x
printMark (JsonString x) _ = x
printMark (JsonArray []) _ = ""
printMark (JsonArray (x:xs)) nb = printMark x (nb + 1)
    ++ printMarkArray xs (nb + 1)
printMark (JsonObject x) nb = printMark' (JsonObject x) nb

printMarkHeaderObject :: [(String, JsonValue)] -> String
printMarkHeaderObject [] = ""
printMarkHeaderObject ((x, y):xs) = x ++ ": "
    ++ printMark y 0 ++ "\n" ++ printMarkHeaderObject xs

printMarkHeader :: String -> JsonValue -> String
printMarkHeader _ (JsonObject y) = "---\n"
    ++ printMarkHeaderObject y ++ "---\n"

printMarkBody :: [(String, JsonValue)] -> String
printMarkBody [] = ""
printMarkBody ((_, y):xs) = printMark y 0 ++ "\n" ++ printMarkObject xs 0

printMarkFile :: JsonValue -> String
printMarkFile (JsonObject ((x, y):xs)) = printMarkHeader x y
    ++ printMarkBody xs
