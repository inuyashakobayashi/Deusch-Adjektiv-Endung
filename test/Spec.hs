{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import DeclensionRules
import GHC.Generics
import Parser
import Prozess
import System.IO
import Types
import Utils
import Validate

-- Bestehende processPhrase Funktion bleibt unverändert
processPhrase :: String -> [GermanNoun] -> Either String String
processPhrase input nouns = do
  parsed <- parseInput input
  (art, adj, noun, nounStr) <- validateNoun nouns parsed
  let nounForm = determineNounForm noun nounStr art
  let phrase =
        AdjectivePhrase
          { article = art,
            adjective = adj,
            noun = noun,
            nounStr = nounStr,
            articleType = if art == Nothing then NoArticle else Definite,
            case_ = Nominative,
            number = case nounForm of
              PluralForm -> Plural
              _ -> Singular,
            nounForm = nounForm
          }
  let processedPhrase = preprocessPhrase phrase
  let ending = getAdjectiveEnding processedPhrase
  return $ case art of
    Nothing -> adj ++ ending ++ " " ++ nounStr
    Just article -> article ++ " " ++ adj ++ ending ++ " " ++ nounStr

-- Funktion zum Laden der Nomen aus JSON
loadNouns :: FilePath -> IO [GermanNoun]
loadNouns filePath = do
  jsonData <- B.readFile filePath
  case decode jsonData of
    Nothing -> error "Fehler beim Parsen der JSON-Datei"
    Just nouns -> return nouns

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8

  -- Lade Nomen aus der JSON-Datei
  testNouns <- loadNouns "german_nouns.json"

  -- Führe Tests mit geladenen Nomen durch
  hspec $ do
    describe "Krasse Adjektivendungen Tests - Genitiv" $ do
      it "Des Bruders mit Artikel" $ do
        putStrLn "\nTest: 'des cool Bruders'"
        putStrLn $ "Eingabe: des cool Bruders"
        putStrLn $ "Erwartet: des coolen Bruders"
        let result = processPhrase "des cool Bruders" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "des coolen Bruders"

      it "Des Homies mit Artikel" $ do
        putStrLn "\nTest: 'des wild Homies'"
        putStrLn $ "Eingabe: des wild Homies"
        putStrLn $ "Erwartet: des wilden Homies"
        let result = processPhrase "des wild Homies" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "des wilden Homies"

      it "Der Schwester mit Artikel" $ do
        putStrLn "\nTest: 'der krass Schwester'"
        putStrLn $ "Eingabe: der krass Schwester"
        putStrLn $ "Erwartet: der krassen Schwester"
        let result = processPhrase "der krass Schwester" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "der krassen Schwester"

      it "Eines Autos mit unbestimmtem Artikel" $ do
        putStrLn "\nTest: 'eines lit Autos'"
        putStrLn $ "Eingabe: eines lit Autos"
        putStrLn $ "Erwartet: eines liten Autos"
        let result = processPhrase "eines lit Autos" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "eines liten Autos"