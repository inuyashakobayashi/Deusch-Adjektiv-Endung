module Utils where

import Data.Char (isLower, isUpper, toUpper)

cleanInput :: String -> String
cleanInput = unwords . words

-- diese Funktion dient zu überpüfen ,ob das erste Buchstaben  von noun Großgeschrieben oder nicht
isCapitalized :: String -> Bool
isCapitalized [] = False
isCapitalized (x : xs) = isUpper x && all isLower xs

-- diese Funktion prüfen,ob alle Buchstaben keingeschrieben ist
isAllLower :: String -> Bool
isAllLower = all isLower

-- diese Funktion macht erste Buchstaben von Wörter großgeschrieben
capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x : xs) = toUpper x : xs

-- diese Funktion prüfen ,ob die Eingaben von benutzer im Form von Maybe art ,adjektiv,noun ist,falls nicht gibt den Fehlermedlung aus
-- wenn ja gibt die Original
validateAndCorrect :: (Maybe String, String, String) -> Either String (Maybe String, String, String)
validateAndCorrect (art, adj, noun)
  | not (isAllLower adj) = Left "adjektiv muss klein schreiben sein"
  | not (isCapitalized noun) = Left $ "head of noun muss groß schreiben sein：" ++ capitalizeFirst noun ++ "?"
  | otherwise = Right (art, adj, capitalizeFirst noun)
