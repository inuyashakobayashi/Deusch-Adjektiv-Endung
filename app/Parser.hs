module Parser
  ( parseInput,
    lookupNoun,
    validateNoun,
    isArticlePlural,
    getOriginalNoun,
  )
where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Types
import Utils

-- | Extrahiert das Nomen aus der Eingabe
getOriginalNoun :: String -> T.Text
getOriginalNoun input = case words (cleanInput input) of
  [_, nounPart] -> T.pack nounPart
  [_, _, nounPart] -> T.pack nounPart
  _ -> T.empty

-- | Prüft ob der Artikel plural ist
isArticlePlural :: Maybe String -> Gender -> Bool
isArticlePlural Nothing _ = False
isArticlePlural (Just "die") Feminine = False
isArticlePlural (Just "die") _ = True
isArticlePlural _ _ = False

-- | Extrahiert das letzte Wort aus der Genitivform
extractLastWord :: T.Text -> T.Text
extractLastWord text = last $ T.words text

-- | Sucht ein Nomen in der Datenbank
lookupNoun :: T.Text -> [GermanNoun] -> Maybe GermanNoun
lookupNoun searchTerm nouns =
  case listToMaybe $ filter (\dbNoun -> word dbNoun == searchTerm) nouns of
    Just foundNoun -> Just foundNoun
    Nothing -> case listToMaybe $ filter (\dbNoun -> plural dbNoun == Just searchTerm) nouns of
      Just foundNoun -> Just foundNoun
      Nothing -> listToMaybe $ filter (\dbNoun -> extractLastWord (genitive dbNoun) == searchTerm) nouns

-- | Hauptparser für die Eingabe
parseInput :: String -> Either String (Maybe String, String, String)
parseInput input = case words (cleanInput input) of
  [] -> Left "Tipp: Bitte geben Sie ein Adjektiv und ein Nomen ein.\nBeispiel: 'gut Haus' oder 'das groß Haus'"
  [inputWord] ->
    Left $
      "Tipp: '"
        ++ inputWord
        ++ "' ist unvollständig.\n"
        ++ "Bitte geben Sie ein Adjektiv und ein Nomen ein.\n"
        ++ "Beispiele:\n- gut Haus\n- das groß Haus"
  [adj, nounPart] -> Right (Nothing, adj, nounPart)
  [art, adj, nounPart] -> Right (Just art, adj, nounPart)
  _ -> Left "Tipp: Bitte geben Sie maximal drei Wörter ein.\nFormat: [Artikel] Adjektiv Nomen\nBeispiel: 'das groß Haus'"

-- | Validierung des Nomens
validateNoun :: [GermanNoun] -> (Maybe String, String, String) -> Either String (Maybe String, String, GermanNoun, String)
validateNoun nounDatabase (art, adj, inputNounStr) =
  case lookupNoun (T.pack inputNounStr) nounDatabase of
    Nothing ->
      Left $
        "Das Nomen '"
          ++ inputNounStr
          ++ "' wurde nicht gefunden.\n"
          ++ "Verfügbare Nomen sind zum Beispiel:\n"
          ++ showExampleNouns (take 3 nounDatabase)
    Just dbNoun -> Right (art, adj, dbNoun, inputNounStr)

-- | Zeigt Beispielnomen an
showExampleNouns :: [GermanNoun] -> String
showExampleNouns nounList = unlines $ map (\dbNoun -> "- " ++ T.unpack (word dbNoun)) nounList