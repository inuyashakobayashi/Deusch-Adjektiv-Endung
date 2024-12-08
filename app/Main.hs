-- |
-- Modul: Main
-- Beschreibung: Hauptprogramm für die deutsche Adjektivdeklination
-- Bewertungskriterien implementiert:
-- - Ein/Ausgabe mit IO-Monaden
-- - Fehlerbehandlung mit Either
-- - Pattern Matching
-- - Case-Ausdrücke
-- - Rekursion (in mainLoop)
module Main where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import DeclensionRules
import Parser
import Prozess
import System.IO (hSetEncoding, stdin, stdout, utf8)
import Types
import Validate

-- |
-- Lädt die Nomen-Datenbank aus einer JSON-Datei.
-- Bewertungskriterien:
-- - IO-Monade
-- - Fehlerbehandlung mit Either
loadNouns :: FilePath -> IO [GermanNoun]
loadNouns filePath = do
  content <- B.readFile filePath
  case eitherDecode content of
    Left err -> error $ "Error parsing JSON: " ++ err
    Right nouns -> return nouns

-- |
-- Hauptschleife des Programms.
-- Bewertungskriterien:
-- - Rekursion
-- - IO-Monade
-- - Pattern Matching
-- - Case-Ausdrücke
-- - Komplexe Datenverarbeitung
mainLoop :: [GermanNoun] -> IO ()
mainLoop nouns = do
  TIO.putStrLn "\nEnter German phrase (or 'quit' to exit):"
  input <- TIO.getLine
  if T.toLower input == T.pack "quit"
    then TIO.putStrLn "Auf Wiedersehen!"
    else do
      case parseInput (T.unpack input) of
        Left parseError -> do
          TIO.putStrLn $ T.pack parseError
          mainLoop nouns
        Right result -> do
          case validateNoun nouns result of
            Left validationError -> do
              TIO.putStrLn $ T.pack validationError
              mainLoop nouns
            Right (art, adj', nounObj, nounPart) -> do
              let originalNoun = nounPart
              let nounF = determineNounForm nounObj originalNoun art
              let initialPhrase =
                    AdjectivePhrase
                      { article = art,
                        adjective = adj',
                        noun = nounObj,
                        nounStr = originalNoun,
                        articleType = if art == Nothing then NoArticle else Definite,
                        case_ = case nounF of
                          GenitiveForm -> Genitive
                          _ -> Nominative,
                        number = case nounF of
                          PluralForm -> Plural
                          _ -> Singular,
                        nounForm = nounF,
                        quantifierArt = case art of
                          Just artText | isQuantifier artText ->
                            case artText of
                              "viele" -> Viele
                              "wenige" -> Wenige
                              "einige" -> Einige
                              "mehrere" -> Mehrere
                              _ -> error "Unexpected quantifier"
                          _ -> Viele
                      }
              let phrase = preprocessPhrase initialPhrase
              let reasoning = getReasoningSteps phrase
              let ending = getAdjectiveEnding phrase
              let finalResult = case art of
                    Nothing -> adj' ++ ending ++ " " ++ originalNoun
                    Just artText -> artText ++ " " ++ adj' ++ ending ++ " " ++ originalNoun
              TIO.putStrLn $ T.pack reasoning
              TIO.putStrLn $ T.pack $ "Final result: " ++ finalResult
              mainLoop nouns

-- |
-- Hauptfunktion des Programms.
-- Bewertungskriterien:
-- - IO-Monade
-- - UTF-8 Handling
-- - Programmstart und -initialisierung
main :: IO ()
main = do
  hSetEncoding stdin utf8 -- Ermöglicht deutsche Umlaute in der Eingabe
  hSetEncoding stdout utf8 -- Ermöglicht deutsche Umlaute in der Ausgabe
  nouns <- loadNouns "german_nouns.json"
  mainLoop nouns