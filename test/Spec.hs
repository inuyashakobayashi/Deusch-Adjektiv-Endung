{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import DeclensionRules
import Parser
import Prozess
import System.IO
import Test.Hspec
import Types
import Validate

-- Bestehende processPhrase Funktion bleibt unverändert
processPhrase :: String -> [GermanNoun] -> Either String String
processPhrase input nouns = do
  parsed <- parseInput input
  (art, adj, nounObj, nounPart) <- validateNoun nouns parsed
  let nounF = determineNounForm nounObj nounPart art
  let phrase =
        AdjectivePhrase
          { article = art,
            adjective = adj,
            noun = nounObj,
            nounStr = nounPart,
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
  let processedPhrase = preprocessPhrase phrase
  let ending = getAdjectiveEnding processedPhrase
  return $ case art of
    Nothing -> adj ++ ending ++ " " ++ nounPart
    Just artText -> artText ++ " " ++ adj ++ ending ++ " " ++ nounPart

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
    describe "Krasse Adjektivendungen Test-G" $ do
      it "Des Bruders mit Artikel" $ do
        putStrLn "\nTest: 'des cool Bruders'"
        putStrLn $ "Eingabe: des cool Bruders"
        putStrLn $ "Erwartet: des coolen Bruders"
        let result = processPhrase "des cool Bruders" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "des coolen Bruders"

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
      ----m D
      it "Dem Mann mit Artikel" $ do
        putStrLn "\nTest: 'dem alt Mann'"
        putStrLn $ "Eingabe: dem alt Mann"
        putStrLn $ "Erwartet: dem alten Mann"
        let result = processPhrase "dem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dem alten Mann"

      it "Einem Mann mit Artikel" $ do
        putStrLn "\nTest: 'einem alt Mann'"
        putStrLn $ "Eingabe: einem alt Mann"
        putStrLn $ "Erwartet: einem alten Mann"
        let result = processPhrase "einem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "einem alten Mann"

      it "Diesem Mann mit Artikel" $ do
        putStrLn "\nTest: 'diesem alt Mann'"
        putStrLn $ "Eingabe: diesem alt Mann"
        putStrLn $ "Erwartet: diesem alten Mann"
        let result = processPhrase "diesem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "diesem alten Mann"

      it "Jedem Mann mit Artikel" $ do
        putStrLn "\nTest: 'jedem alt Mann'"
        putStrLn $ "Eingabe: jedem alt Mann"
        putStrLn $ "Erwartet: jedem alten Mann"
        let result = processPhrase "jedem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jedem alten Mann"

      it "Allen Stoffwechselprodukte mit Artikel" $ do
        putStrLn "\nTest: 'allen streng Stoffwechselprodukte'"
        putStrLn $ "Eingabe: allen streng Stoffwechselprodukte"
        putStrLn $ "Erwartet: allen strengen Stoffwechselprodukte"
        let result = processPhrase "allen streng Stoffwechselprodukte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "allen strengen Stoffwechselprodukte"

      it "Beiden Stoffwechselprodukte mit Artikel" $ do
        putStrLn "\nTest: 'beiden streng Stoffwechselprodukte'"
        putStrLn $ "Eingabe: beiden streng Stoffwechselprodukte"
        putStrLn $ "Erwartet: beiden strengen Stoffwechselprodukte"
        let result = processPhrase "beiden streng Stoffwechselprodukte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "beiden strengen Stoffwechselprodukte"

      it "Meinem Mann mit Artikel" $ do
        putStrLn "\nTest: 'meinem alt Mann'"
        putStrLn $ "Eingabe: meinem alt Mann"
        putStrLn $ "Erwartet: meinem alten Mann"
        let result = processPhrase "meinem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meinem alten Mann"

      it "Deinem Mann mit Artikel" $ do
        putStrLn "\nTest: 'deinem alt Mann'"
        putStrLn $ "Eingabe: deinem alt Mann"
        putStrLn $ "Erwartet: deinem alten Mann"
        let result = processPhrase "deinem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deinem alten Mann"

      it "Keinem Mann mit Artikel" $ do
        putStrLn "\nTest: 'keinem alt Mann'"
        putStrLn $ "Eingabe: keinem alt Mann"
        putStrLn $ "Erwartet: keinem alten Mann"
        let result = processPhrase "keinem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keinem alten Mann"

      it "Manchem Mann mit Artikel" $ do
        putStrLn "\nTest: 'manchem alt Mann'"
        putStrLn $ "Eingabe: manchem alt Mann"
        putStrLn $ "Erwartet: manchem alten Mann"
        let result = processPhrase "manchem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manchem alten Mann"

      it "Solchem Mann mit Artikel" $ do
        putStrLn "\nTest: 'solchem alt Mann'"
        putStrLn $ "Eingabe: solchem alt Mann"
        putStrLn $ "Erwartet: solchem alten Mann"
        let result = processPhrase "solchem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solchem alten Mann"

      it "Welchem Mann mit Artikel" $ do
        putStrLn "\nTest: 'welchem alt Mann'"
        putStrLn $ "Eingabe: welchem alt Mann"
        putStrLn $ "Erwartet: welchem alten Mann"
        let result = processPhrase "welchem alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welchem alten Mann"

      -- n D
      it "Dem Bild mit Artikel" $ do
        putStrLn "\nTest: 'dem alt Bild'"
        putStrLn $ "Eingabe: dem alt Bild"
        putStrLn $ "Erwartet: dem alten Bild"
        let result = processPhrase "dem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dem alten Bild"

      it "Einem Bild mit Artikel" $ do
        putStrLn "\nTest: 'einem alt Bild'"
        putStrLn $ "Eingabe: einem alt Bild"
        putStrLn $ "Erwartet: einem alten Bild"
        let result = processPhrase "einem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "einem alten Bild"

      it "Diesem Bild mit Artikel" $ do
        putStrLn "\nTest: 'diesem alt Bild'"
        putStrLn $ "Eingabe: diesem alt Bild"
        putStrLn $ "Erwartet: diesem alten Bild"
        let result = processPhrase "diesem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "diesem alten Bild"

      it "Jedem Bild mit Artikel" $ do
        putStrLn "\nTest: 'jedem alt Bild'"
        putStrLn $ "Eingabe: jedem alt Bild"
        putStrLn $ "Erwartet: jedem alten Bild"
        let result = processPhrase "jedem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jedem alten Bild"

      it "Meinem Bild mit Artikel" $ do
        putStrLn "\nTest: 'meinem alt Bild'"
        putStrLn $ "Eingabe: meinem alt Bild"
        putStrLn $ "Erwartet: meinem alten Bild"
        let result = processPhrase "meinem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meinem alten Bild"

      it "Deinem Bild mit Artikel" $ do
        putStrLn "\nTest: 'deinem alt Bild'"
        putStrLn $ "Eingabe: deinem alt Bild"
        putStrLn $ "Erwartet: deinem alten Bild"
        let result = processPhrase "deinem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deinem alten Bild"

      it "Keinem Bild mit Artikel" $ do
        putStrLn "\nTest: 'keinem alt Bild'"
        putStrLn $ "Eingabe: keinem alt Bild"
        putStrLn $ "Erwartet: keinem alten Bild"
        let result = processPhrase "keinem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keinem alten Bild"

      it "Manchem Bild mit Artikel" $ do
        putStrLn "\nTest: 'manchem alt Bild'"
        putStrLn $ "Eingabe: manchem alt Bild"
        putStrLn $ "Erwartet: manchem alten Bild"
        let result = processPhrase "manchem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manchem alten Bild"

      it "Solchem Bild mit Artikel" $ do
        putStrLn "\nTest: 'solchem alt Bild'"
        putStrLn $ "Eingabe: solchem alt Bild"
        putStrLn $ "Erwartet: solchem alten Bild"
        let result = processPhrase "solchem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solchem alten Bild"

      it "Welchem Bild mit Artikel" $ do
        putStrLn "\nTest: 'welchem alt Bild'"
        putStrLn $ "Eingabe: welchem alt Bild"
        putStrLn $ "Erwartet: welchem alten Bild"
        let result = processPhrase "welchem alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welchem alten Bild"
      -- f. D

      it "Der Animation mit Artikel" $ do
        putStrLn "\nTest: 'der alt Animation'"
        putStrLn $ "Eingabe: der alt Animation"
        putStrLn $ "Erwartet: der alten Animation"
        let result = processPhrase "der alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "der alten Animation"

      it "Einer Animation mit Artikel" $ do
        putStrLn "\nTest: 'einer alt Animation'"
        putStrLn $ "Eingabe: einer alt Animation"
        putStrLn $ "Erwartet: einer alten Animation"
        let result = processPhrase "einer alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "einer alten Animation"

      it "Dieser Animation mit Artikel" $ do
        putStrLn "\nTest: 'dieser alt Animation'"
        putStrLn $ "Eingabe: dieser alt Animation"
        putStrLn $ "Erwartet: dieser alten Animation"
        let result = processPhrase "dieser alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dieser alten Animation"

      it "Jeder Animation mit Artikel" $ do
        putStrLn "\nTest: 'jeder alt Animation'"
        putStrLn $ "Eingabe: jeder alt Animation"
        putStrLn $ "Erwartet: jeder alten Animation"
        let result = processPhrase "jeder alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jeder alten Animation"

      it "Meiner Animation mit Artikel" $ do
        putStrLn "\nTest: 'meiner alt Animation'"
        putStrLn $ "Eingabe: meiner alt Animation"
        putStrLn $ "Erwartet: meiner alten Animation"
        let result = processPhrase "meiner alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meiner alten Animation"

      it "Deiner Animation mit Artikel" $ do
        putStrLn "\nTest: 'deiner alt Animation'"
        putStrLn $ "Eingabe: deiner alt Animation"
        putStrLn $ "Erwartet: deiner alten Animation"
        let result = processPhrase "deiner alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deiner alten Animation"

      it "Keiner Animation mit Artikel" $ do
        putStrLn "\nTest: 'keiner alt Animation'"
        putStrLn $ "Eingabe: keiner alt Animation"
        putStrLn $ "Erwartet: keiner alten Animation"
        let result = processPhrase "keiner alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keiner alten Animation"

      it "Mancher Animation mit Artikel" $ do
        putStrLn "\nTest: 'mancher alt Animation'"
        putStrLn $ "Eingabe: mancher alt Animation"
        putStrLn $ "Erwartet: mancher alten Animation"
        let result = processPhrase "mancher alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "mancher alten Animation"

      it "Solcher Animation mit Artikel" $ do
        putStrLn "\nTest: 'solcher alt Animation'"
        putStrLn $ "Eingabe: solcher alt Animation"
        putStrLn $ "Erwartet: solcher alten Animation"
        let result = processPhrase "solcher alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solcher alten Animation"

      it "Welcher Animation mit Artikel" $ do
        putStrLn "\nTest: 'welcher alt Animation'"
        putStrLn $ "Eingabe: welcher alt Animation"
        putStrLn $ "Erwartet: welcher alten Animation"
        let result = processPhrase "welcher alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welcher alten Animation"

      -- pl D
      it "Den Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'den alt Widerparte'"
        putStrLn $ "Eingabe: den alt Widerparte"
        putStrLn $ "Erwartet: den alten Widerparte"
        let result = processPhrase "den alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "den alten Widerparte"

      it "Diesen Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'diesen alt Widerparte'"
        putStrLn $ "Eingabe: diesen alt Widerparte"
        putStrLn $ "Erwartet: diesen alten Widerparte"
        let result = processPhrase "diesen alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "diesen alten Widerparte"

      it "Jeden Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'jeden alt Widerparte'"
        putStrLn $ "Eingabe: jeden alt Widerparte"
        putStrLn $ "Erwartet: jeden alten Widerparte"
        let result = processPhrase "jeden alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jeden alten Widerparte"

      it "Meinen Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'meinen alt Widerparte'"
        putStrLn $ "Eingabe: meinen alt Widerparte"
        putStrLn $ "Erwartet: meinen alten Widerparte"
        let result = processPhrase "meinen alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meinen alten Widerparte"

      it "Deinen Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'deinen alt Widerparte'"
        putStrLn $ "Eingabe: deinen alt Widerparte"
        putStrLn $ "Erwartet: deinen alten Widerparte"
        let result = processPhrase "deinen alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deinen alten Widerparte"

      it "Keinen Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'keinen alt Widerparte'"
        putStrLn $ "Eingabe: keinen alt Widerparte"
        putStrLn $ "Erwartet: keinen alten Widerparte"
        let result = processPhrase "keinen alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keinen alten Widerparte"

      it "Manchen Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'manchen alt Widerparte'"
        putStrLn $ "Eingabe: manchen alt Widerparte"
        putStrLn $ "Erwartet: manchen alten Widerparte"
        let result = processPhrase "manchen alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manchen alten Widerparte"

      it "Solchen Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'solchen alt Widerparte'"
        putStrLn $ "Eingabe: solchen alt Widerparte"
        putStrLn $ "Erwartet: solchen alten Widerparte"
        let result = processPhrase "solchen alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solchen alten Widerparte"

      it "Welchen Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'welchen alt Widerparte'"
        putStrLn $ "Eingabe: welchen alt Widerparte"
        putStrLn $ "Erwartet: welchen alten Widerparte"
        let result = processPhrase "welchen alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welchen alten Widerparte"
      -- A

      --       describe "Krasse Adjektivendungen Tests - Akk $ do
      -- pl A
      it "Die Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'die alt Widerparte'"
        putStrLn $ "Eingabe: die alt Widerparte"
        putStrLn $ "Erwartet: die alten Widerparte"
        let result = processPhrase "die alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "die alten Widerparte"

      it "Diese Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'diese alt Widerparte'"
        putStrLn $ "Eingabe: diese alt Widerparte"
        putStrLn $ "Erwartet: diese alten Widerparte"
        let result = processPhrase "diese alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "diese alten Widerparte"

      it "Jede Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'jede alt Widerparte'"
        putStrLn $ "Eingabe: jede alt Widerparte"
        putStrLn $ "Erwartet: jede alten Widerparte"
        let result = processPhrase "jede alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jede alten Widerparte"

      it "Meine Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'meine alt Widerparte'"
        putStrLn $ "Eingabe: meine alt Widerparte"
        putStrLn $ "Erwartet: meine alten Widerparte"
        let result = processPhrase "meine alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meine alten Widerparte"

      it "Deine Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'deine alt Widerparte'"
        putStrLn $ "Eingabe: deine alt Widerparte"
        putStrLn $ "Erwartet: deine alten Widerparte"
        let result = processPhrase "deine alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deine alten Widerparte"

      it "Keine Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'keine alt Widerparte'"
        putStrLn $ "Eingabe: keine alt Widerparte"
        putStrLn $ "Erwartet: keine alten Widerparte"
        let result = processPhrase "keine alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keine alten Widerparte"

      it "Manche Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'manche alt Widerparte'"
        putStrLn $ "Eingabe: manche alt Widerparte"
        putStrLn $ "Erwartet: manche alten Widerparte"
        let result = processPhrase "manche alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manche alten Widerparte"

      it "Solche Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'solche alt Widerparte'"
        putStrLn $ "Eingabe: solche alt Widerparte"
        putStrLn $ "Erwartet: solche alten Widerparte"
        let result = processPhrase "solche alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solche alten Widerparte"

      it "Welche Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'welche alt Widerparte'"
        putStrLn $ "Eingabe: welche alt Widerparte"
        putStrLn $ "Erwartet: welche alten Widerparte"
        let result = processPhrase "welche alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welche alten Widerparte"

      it "Alle Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'alle alt Widerparte'"
        putStrLn $ "Eingabe: alle alt Widerparte"
        putStrLn $ "Erwartet: alle alten Widerparte"
        let result = processPhrase "alle alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "alle alten Widerparte"

      it "Beide Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'beide alt Widerparte'"
        putStrLn $ "Eingabe: beide alt Widerparte"
        putStrLn $ "Erwartet: beide alten Widerparte"
        let result = processPhrase "beide alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "beide alten Widerparte"

      --              m. A
      it "Den Mann mit Artikel" $ do
        putStrLn "\nTest: 'den alt Mann'"
        putStrLn $ "Eingabe: den alt Mann"
        putStrLn $ "Erwartet: den alten Mann"
        let result = processPhrase "den alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "den alten Mann"

      it "Einen Mann mit Artikel" $ do
        putStrLn "\nTest: 'einen alt Mann'"
        putStrLn $ "Eingabe: einen alt Mann"
        putStrLn $ "Erwartet: einen alten Mann"
        let result = processPhrase "einen alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "einen alten Mann"

      it "Diesen Mann mit Artikel" $ do
        putStrLn "\nTest: 'diesen alt Mann'"
        putStrLn $ "Eingabe: diesen alt Mann"
        putStrLn $ "Erwartet: diesen alten Mann"
        let result = processPhrase "diesen alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "diesen alten Mann"

      it "Jeden Mann mit Artikel" $ do
        putStrLn "\nTest: 'jeden alt Mann'"
        putStrLn $ "Eingabe: jeden alt Mann"
        putStrLn $ "Erwartet: jeden alten Mann"
        let result = processPhrase "jeden alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jeden alten Mann"

      it "Meinen Mann mit Artikel" $ do
        putStrLn "\nTest: 'meinen alt Mann'"
        putStrLn $ "Eingabe: meinen alt Mann"
        putStrLn $ "Erwartet: meinen alten Mann"
        let result = processPhrase "meinen alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meinen alten Mann"

      it "Deinen Mann mit Artikel" $ do
        putStrLn "\nTest: 'deinen alt Mann'"
        putStrLn $ "Eingabe: deinen alt Mann"
        putStrLn $ "Erwartet: deinen alten Mann"
        let result = processPhrase "deinen alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deinen alten Mann"

      it "Keinen Mann mit Artikel" $ do
        putStrLn "\nTest: 'keinen alt Mann'"
        putStrLn $ "Eingabe: keinen alt Mann"
        putStrLn $ "Erwartet: keinen alten Mann"
        let result = processPhrase "keinen alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keinen alten Mann"

      it "Manchen Mann mit Artikel" $ do
        putStrLn "\nTest: 'manchen alt Mann'"
        putStrLn $ "Eingabe: manchen alt Mann"
        putStrLn $ "Erwartet: manchen alten Mann"
        let result = processPhrase "manchen alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manchen alten Mann"

      it "Solchen Mann mit Artikel" $ do
        putStrLn "\nTest: 'solchen alt Mann'"
        putStrLn $ "Eingabe: solchen alt Mann"
        putStrLn $ "Erwartet: solchen alten Mann"
        let result = processPhrase "solchen alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solchen alten Mann"

      it "Welchen Mann mit Artikel" $ do
        putStrLn "\nTest: 'welchen alt Mann'"
        putStrLn $ "Eingabe: welchen alt Mann"
        putStrLn $ "Erwartet: welchen alten Mann"
        let result = processPhrase "welchen alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welchen alten Mann"
      --            n A
      it "Das Bild mit Artikel" $ do
        putStrLn "\nTest: 'das alt Bild'"
        putStrLn $ "Eingabe: das alt Bild"
        putStrLn $ "Erwartet: das alte Bild"
        let result = processPhrase "das alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "das alte Bild"

      it "Ein Bild mit Artikel" $ do
        putStrLn "\nTest: 'ein alt Bild'"
        putStrLn $ "Eingabe: ein alt Bild"
        putStrLn $ "Erwartet: ein altes Bild"
        let result = processPhrase "ein alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "ein altes Bild"

      it "Dieses Bild mit Artikel" $ do
        putStrLn "\nTest: 'dieses alt Bild'"
        putStrLn $ "Eingabe: dieses alt Bild"
        putStrLn $ "Erwartet: dieses alte Bild"
        let result = processPhrase "dieses alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dieses alte Bild"

      it "Jedes Bild mit Artikel" $ do
        putStrLn "\nTest: 'jedes alt Bild'"
        putStrLn $ "Eingabe: jedes alt Bild"
        putStrLn $ "Erwartet: jedes alte Bild"
        let result = processPhrase "jedes alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jedes alte Bild"

      it "Mein Bild mit Artikel" $ do
        putStrLn "\nTest: 'mein alt Bild'"
        putStrLn $ "Eingabe: mein alt Bild"
        putStrLn $ "Erwartet: mein altes Bild"
        let result = processPhrase "mein alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "mein altes Bild"

      it "Dein Bild mit Artikel" $ do
        putStrLn "\nTest: 'dein alt Bild'"
        putStrLn $ "Eingabe: dein alt Bild"
        putStrLn $ "Erwartet: dein altes Bild"
        let result = processPhrase "dein alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dein altes Bild"

      it "Kein Bild mit Artikel" $ do
        putStrLn "\nTest: 'kein alt Bild'"
        putStrLn $ "Eingabe: kein alt Bild"
        putStrLn $ "Erwartet: kein altes Bild"
        let result = processPhrase "kein alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "kein altes Bild"

      it "Manches Bild mit Artikel" $ do
        putStrLn "\nTest: 'manches alt Bild'"
        putStrLn $ "Eingabe: manches alt Bild"
        putStrLn $ "Erwartet: manches alte Bild"
        let result = processPhrase "manches alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manches alte Bild"

      it "Solches Bild mit Artikel" $ do
        putStrLn "\nTest: 'solches alt Bild'"
        putStrLn $ "Eingabe: solches alt Bild"
        putStrLn $ "Erwartet: solches alte Bild"
        let result = processPhrase "solches alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solches alte Bild"

      it "Welches Bild mit Artikel" $ do
        putStrLn "\nTest: 'welches alt Bild'"
        putStrLn $ "Eingabe: welches alt Bild"
        putStrLn $ "Erwartet: welches alte Bild"
        let result = processPhrase "welches alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welches alte Bild"
      -- f A

      it "Die Animation mit Artikel" $ do
        putStrLn "\nTest: 'die alt Animation'"
        putStrLn $ "Eingabe: die alt Animation"
        putStrLn $ "Erwartet: die alte Animation"
        let result = processPhrase "die alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "die alte Animation"

      it "Eine Animation mit Artikel" $ do
        putStrLn "\nTest: 'eine alt Animation'"
        putStrLn $ "Eingabe: eine alt Animation"
        putStrLn $ "Erwartet: eine alte Animation"
        let result = processPhrase "eine alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "eine alte Animation"

      it "Diese Animation mit Artikel" $ do
        putStrLn "\nTest: 'diese alt Animation'"
        putStrLn $ "Eingabe: diese alt Animation"
        putStrLn $ "Erwartet: diese alte Animation"
        let result = processPhrase "diese alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "diese alte Animation"

      it "Jede Animation mit Artikel" $ do
        putStrLn "\nTest: 'jede alt Animation'"
        putStrLn $ "Eingabe: jede alt Animation"
        putStrLn $ "Erwartet: jede alte Animation"
        let result = processPhrase "jede alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jede alte Animation"

      it "Meine Animation mit Artikel" $ do
        putStrLn "\nTest: 'meine alt Animation'"
        putStrLn $ "Eingabe: meine alt Animation"
        putStrLn $ "Erwartet: meine alte Animation"
        let result = processPhrase "meine alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meine alte Animation"

      it "Deine Animation mit Artikel" $ do
        putStrLn "\nTest: 'deine alt Animation'"
        putStrLn $ "Eingabe: deine alt Animation"
        putStrLn $ "Erwartet: deine alte Animation"
        let result = processPhrase "deine alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deine alte Animation"

      it "Keine Animation mit Artikel" $ do
        putStrLn "\nTest: 'keine alt Animation'"
        putStrLn $ "Eingabe: keine alt Animation"
        putStrLn $ "Erwartet: keine alte Animation"
        let result = processPhrase "keine alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keine alte Animation"

      it "Manche Animation mit Artikel" $ do
        putStrLn "\nTest: 'manche alt Animation'"
        putStrLn $ "Eingabe: manche alt Animation"
        putStrLn $ "Erwartet: manche alte Animation"
        let result = processPhrase "manche alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manche alte Animation"

      it "Solche Animation mit Artikel" $ do
        putStrLn "\nTest: 'solche alt Animation'"
        putStrLn $ "Eingabe: solche alt Animation"
        putStrLn $ "Erwartet: solche alte Animation"
        let result = processPhrase "solche alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solche alte Animation"

      it "Welche Animation mit Artikel" $ do
        putStrLn "\nTest: 'welche alt Animation'"
        putStrLn $ "Eingabe: welche alt Animation"
        putStrLn $ "Erwartet: welche alte Animation"
        let result = processPhrase "welche alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welche alte Animation"
      --- N.
      -- pl.
      it "Die Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'die alt Widerparte'"
        putStrLn $ "Eingabe: die alt Widerparte"
        putStrLn $ "Erwartet: die alten Widerparte"
        let result = processPhrase "die alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "die alten Widerparte"

      it "Diese Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'diese alt Widerparte'"
        putStrLn $ "Eingabe: diese alt Widerparte"
        putStrLn $ "Erwartet: diese alten Widerparte"
        let result = processPhrase "diese alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "diese alten Widerparte"

      it "Jede Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'jede alt Widerparte'"
        putStrLn $ "Eingabe: jede alt Widerparte"
        putStrLn $ "Erwartet: jede alten Widerparte"
        let result = processPhrase "jede alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jede alten Widerparte"

      it "Meine Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'meine alt Widerparte'"
        putStrLn $ "Eingabe: meine alt Widerparte"
        putStrLn $ "Erwartet: meine alten Widerparte"
        let result = processPhrase "meine alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meine alten Widerparte"

      it "Deine Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'deine alt Widerparte'"
        putStrLn $ "Eingabe: deine alt Widerparte"
        putStrLn $ "Erwartet: deine alten Widerparte"
        let result = processPhrase "deine alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deine alten Widerparte"

      it "Keine Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'keine alt Widerparte'"
        putStrLn $ "Eingabe: keine alt Widerparte"
        putStrLn $ "Erwartet: keine alten Widerparte"
        let result = processPhrase "keine alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keine alten Widerparte"

      it "Manche Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'manche alt Widerparte'"
        putStrLn $ "Eingabe: manche alt Widerparte"
        putStrLn $ "Erwartet: manche alten Widerparte"
        let result = processPhrase "manche alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manche alten Widerparte"

      it "Solche Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'solche alt Widerparte'"
        putStrLn $ "Eingabe: solche alt Widerparte"
        putStrLn $ "Erwartet: solche alten Widerparte"
        let result = processPhrase "solche alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solche alten Widerparte"

      it "Welche Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'welche alt Widerparte'"
        putStrLn $ "Eingabe: welche alt Widerparte"
        putStrLn $ "Erwartet: welche alten Widerparte"
        let result = processPhrase "welche alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welche alten Widerparte"

      it "Alle Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'alle alt Widerparte'"
        putStrLn $ "Eingabe: alle alt Widerparte"
        putStrLn $ "Erwartet: alle alten Widerparte"
        let result = processPhrase "alle alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "alle alten Widerparte"

      it "Beide Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'beide alt Widerparte'"
        putStrLn $ "Eingabe: beide alt Widerparte"
        putStrLn $ "Erwartet: beide alten Widerparte"
        let result = processPhrase "beide alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "beide alten Widerparte"
      -- m N
      it "Der Mann mit Artikel" $ do
        putStrLn "\nTest: 'der alt Mann'"
        putStrLn $ "Eingabe: der alt Mann"
        putStrLn $ "Erwartet: der alte Mann"
        let result = processPhrase "der alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "der alte Mann"

      it "Ein Mann mit Artikel" $ do
        putStrLn "\nTest: 'ein alt Mann'"
        putStrLn $ "Eingabe: ein alt Mann"
        putStrLn $ "Erwartet: ein alter Mann"
        let result = processPhrase "ein alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "ein alter Mann"

      it "Dieser Mann mit Artikel" $ do
        putStrLn "\nTest: 'dieser alt Mann'"
        putStrLn $ "Eingabe: dieser alt Mann"
        putStrLn $ "Erwartet: dieser alte Mann"
        let result = processPhrase "dieser alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dieser alte Mann"

      it "Jeder Mann mit Artikel" $ do
        putStrLn "\nTest: 'jeder alt Mann'"
        putStrLn $ "Eingabe: jeder alt Mann"
        putStrLn $ "Erwartet: jeder alte Mann"
        let result = processPhrase "jeder alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jeder alte Mann"

      it "Mein Mann mit Artikel" $ do
        putStrLn "\nTest: 'mein alt Mann'"
        putStrLn $ "Eingabe: mein alt Mann"
        putStrLn $ "Erwartet: mein alter Mann"
        let result = processPhrase "mein alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "mein alter Mann"

      it "Dein Mann mit Artikel" $ do
        putStrLn "\nTest: 'dein alt Mann'"
        putStrLn $ "Eingabe: dein alt Mann"
        putStrLn $ "Erwartet: dein alter Mann"
        let result = processPhrase "dein alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dein alter Mann"

      it "Kein Mann mit Artikel" $ do
        putStrLn "\nTest: 'kein alt Mann'"
        putStrLn $ "Eingabe: kein alt Mann"
        putStrLn $ "Erwartet: kein alter Mann"
        let result = processPhrase "kein alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "kein alter Mann"

      it "Mancher Mann mit Artikel" $ do
        putStrLn "\nTest: 'mancher alt Mann'"
        putStrLn $ "Eingabe: mancher alt Mann"
        putStrLn $ "Erwartet: mancher alte Mann"
        let result = processPhrase "mancher alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "mancher alte Mann"

      it "Solcher Mann mit Artikel" $ do
        putStrLn "\nTest: 'solcher alt Mann'"
        putStrLn $ "Eingabe: solcher alt Mann"
        putStrLn $ "Erwartet: solcher alte Mann"
        let result = processPhrase "solcher alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solcher alte Mann"

      it "Welcher Mann mit Artikel" $ do
        putStrLn "\nTest: 'welcher alt Mann'"
        putStrLn $ "Eingabe: welcher alt Mann"
        putStrLn $ "Erwartet: welcher alte Mann"
        let result = processPhrase "welcher alt Mann" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welcher alte Mann"
      -- N. f
      it "Die Animation mit Artikel" $ do
        putStrLn "\nTest: 'die alt Animation'"
        putStrLn $ "Eingabe: die alt Animation"
        putStrLn $ "Erwartet: die alte Animation"
        let result = processPhrase "die alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "die alte Animation"

      it "Eine Animation mit Artikel" $ do
        putStrLn "\nTest: 'eine alt Animation'"
        putStrLn $ "Eingabe: eine alt Animation"
        putStrLn $ "Erwartet: eine alte Animation"
        let result = processPhrase "eine alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "eine alte Animation"

      it "Diese Animation mit Artikel" $ do
        putStrLn "\nTest: 'diese alt Animation'"
        putStrLn $ "Eingabe: diese alt Animation"
        putStrLn $ "Erwartet: diese alte Animation"
        let result = processPhrase "diese alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "diese alte Animation"

      it "Jede Animation mit Artikel" $ do
        putStrLn "\nTest: 'jede alt Animation'"
        putStrLn $ "Eingabe: jede alt Animation"
        putStrLn $ "Erwartet: jede alte Animation"
        let result = processPhrase "jede alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jede alte Animation"

      it "Meine Animation mit Artikel" $ do
        putStrLn "\nTest: 'meine alt Animation'"
        putStrLn $ "Eingabe: meine alt Animation"
        putStrLn $ "Erwartet: meine alte Animation"
        let result = processPhrase "meine alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meine alte Animation"

      it "Deine Animation mit Artikel" $ do
        putStrLn "\nTest: 'deine alt Animation'"
        putStrLn $ "Eingabe: deine alt Animation"
        putStrLn $ "Erwartet: deine alte Animation"
        let result = processPhrase "deine alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deine alte Animation"

      it "Keine Animation mit Artikel" $ do
        putStrLn "\nTest: 'keine alt Animation'"
        putStrLn $ "Eingabe: keine alt Animation"
        putStrLn $ "Erwartet: keine alte Animation"
        let result = processPhrase "keine alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keine alte Animation"

      it "Manche Animation mit Artikel" $ do
        putStrLn "\nTest: 'manche alt Animation'"
        putStrLn $ "Eingabe: manche alt Animation"
        putStrLn $ "Erwartet: manche alte Animation"
        let result = processPhrase "manche alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manche alte Animation"

      it "Solche Animation mit Artikel" $ do
        putStrLn "\nTest: 'solche alt Animation'"
        putStrLn $ "Eingabe: solche alt Animation"
        putStrLn $ "Erwartet: solche alte Animation"
        let result = processPhrase "solche alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solche alte Animation"

      it "Welche Animation mit Artikel" $ do
        putStrLn "\nTest: 'welche alt Animation'"
        putStrLn $ "Eingabe: welche alt Animation"
        putStrLn $ "Erwartet: welche alte Animation"
        let result = processPhrase "welche alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welche alte Animation"

      -- N. n
      it "Das Bild mit Artikel" $ do
        putStrLn "\nTest: 'das alt Bild'"
        putStrLn $ "Eingabe: das alt Bild"
        putStrLn $ "Erwartet: das alte Bild"
        let result = processPhrase "das alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "das alte Bild"

      it "Ein Bild mit Artikel" $ do
        putStrLn "\nTest: 'ein alt Bild'"
        putStrLn $ "Eingabe: ein alt Bild"
        putStrLn $ "Erwartet: ein altes Bild"
        let result = processPhrase "ein alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "ein altes Bild"

      it "Dieses Bild mit Artikel" $ do
        putStrLn "\nTest: 'dieses alt Bild'"
        putStrLn $ "Eingabe: dieses alt Bild"
        putStrLn $ "Erwartet: dieses alte Bild"
        let result = processPhrase "dieses alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dieses alte Bild"

      it "Jedes Bild mit Artikel" $ do
        putStrLn "\nTest: 'jedes alt Bild'"
        putStrLn $ "Eingabe: jedes alt Bild"
        putStrLn $ "Erwartet: jedes alte Bild"
        let result = processPhrase "jedes alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jedes alte Bild"

      it "Mein Bild mit Artikel" $ do
        putStrLn "\nTest: 'mein alt Bild'"
        putStrLn $ "Eingabe: mein alt Bild"
        putStrLn $ "Erwartet: mein altes Bild"
        let result = processPhrase "mein alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "mein altes Bild"

      it "Dein Bild mit Artikel" $ do
        putStrLn "\nTest: 'dein alt Bild'"
        putStrLn $ "Eingabe: dein alt Bild"
        putStrLn $ "Erwartet: dein altes Bild"
        let result = processPhrase "dein alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dein altes Bild"

      it "Kein Bild mit Artikel" $ do
        putStrLn "\nTest: 'kein alt Bild'"
        putStrLn $ "Eingabe: kein alt Bild"
        putStrLn $ "Erwartet: kein altes Bild"
        let result = processPhrase "kein alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "kein altes Bild"

      it "Manches Bild mit Artikel" $ do
        putStrLn "\nTest: 'manches alt Bild'"
        putStrLn $ "Eingabe: manches alt Bild"
        putStrLn $ "Erwartet: manches alte Bild"
        let result = processPhrase "manches alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manches alte Bild"

      it "Solches Bild mit Artikel" $ do
        putStrLn "\nTest: 'solches alt Bild'"
        putStrLn $ "Eingabe: solches alt Bild"
        putStrLn $ "Erwartet: solches alte Bild"
        let result = processPhrase "solches alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solches alte Bild"

      it "Welche Bilds mit Artikel" $ do
        putStrLn "\nTest: 'welches alt Bild'"
        putStrLn $ "Eingabe: welches alt Bild"
        putStrLn $ "Erwartet: welches alte Bild"
        let result = processPhrase "welches alt Bild" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welches alte Bild"
      -- G pl
      it "Der Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'der alt Widerparte'"
        putStrLn $ "Eingabe: der alt Widerparte"
        putStrLn $ "Erwartet: der alten Widerparte"
        let result = processPhrase "der alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "der alten Widerparte"

      it "Dieser Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'dieser alt Widerparte'"
        putStrLn $ "Eingabe: dieser alt Widerparte"
        putStrLn $ "Erwartet: dieser alten Widerparte"
        let result = processPhrase "dieser alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dieser alten Widerparte"

      it "Jeder Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'jeder alt Widerparte'"
        putStrLn $ "Eingabe: jeder alt Widerparte"
        putStrLn $ "Erwartet: jeder alten Widerparte"
        let result = processPhrase "jeder alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jeder alten Widerparte"

      it "Meiner Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'meiner alt Widerparte'"
        putStrLn $ "Eingabe: meiner alt Widerparte"
        putStrLn $ "Erwartet: meiner alten Widerparte"
        let result = processPhrase "meiner alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meiner alten Widerparte"

      it "Deiner Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'deiner alt Widerparte'"
        putStrLn $ "Eingabe: deiner alt Widerparte"
        putStrLn $ "Erwartet: deiner alten Widerparte"
        let result = processPhrase "deiner alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deiner alten Widerparte"

      it "Keiner Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'keiner alt Widerparte'"
        putStrLn $ "Eingabe: keiner alt Widerparte"
        putStrLn $ "Erwartet: keiner alten Widerparte"
        let result = processPhrase "keiner alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keiner alten Widerparte"

      it "Mancher Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'mancher alt Widerparte'"
        putStrLn $ "Eingabe: mancher alt Widerparte"
        putStrLn $ "Erwartet: mancher alten Widerparte"
        let result = processPhrase "mancher alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "mancher alten Widerparte"

      it "Solcher Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'solcher alt Widerparte'"
        putStrLn $ "Eingabe: solcher alt Widerparte"
        putStrLn $ "Erwartet: solcher alten Widerparte"
        let result = processPhrase "solcher alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solcher alten Widerparte"

      it "Welcher Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'welcher alt Widerparte'"
        putStrLn $ "Eingabe: welcher alt Widerparte"
        putStrLn $ "Erwartet: welcher alten Widerparte"
        let result = processPhrase "welcher alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welcher alten Widerparte"

      it "Aller Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'aller alt Widerparte'"
        putStrLn $ "Eingabe: aller alt Widerparte"
        putStrLn $ "Erwartet: aller alten Widerparte"
        let result = processPhrase "aller alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "aller alten Widerparte"

      it "Beider Widerparte mit Artikel" $ do
        putStrLn "\nTest: 'beider alt Widerparte'"
        putStrLn $ "Eingabe: beider alt Widerparte"
        putStrLn $ "Erwartet: beider alten Widerparte"
        let result = processPhrase "beider alt Widerparte" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "beider alten Widerparte"
      -- G m
      it "Des Mannes mit Artikel" $ do
        putStrLn "\nTest: 'des alt Mannes'"
        putStrLn $ "Eingabe: des alt Mannes"
        putStrLn $ "Erwartet: des alten Mannes"
        let result = processPhrase "des alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "des alten Mannes"

      it "Eines Mannes mit Artikel" $ do
        putStrLn "\nTest: 'eines alt Mannes'"
        putStrLn $ "Eingabe: eines alt Mannes"
        putStrLn $ "Erwartet: eines alten Mannes"
        let result = processPhrase "eines alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "eines alten Mannes"

      it "Dieses Mannes mit Artikel" $ do
        putStrLn "\nTest: 'dieses alt Mannes'"
        putStrLn $ "Eingabe: dieses alt Mannes"
        putStrLn $ "Erwartet: dieses alten Mannes"
        let result = processPhrase "dieses alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dieses alten Mannes"

      it "Jedes Mannes mit Artikel" $ do
        putStrLn "\nTest: 'jedes alt Mannes'"
        putStrLn $ "Eingabe: jedes alt Mannes"
        putStrLn $ "Erwartet: jedes alten Mannes"
        let result = processPhrase "jedes alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jedes alten Mannes"

      it "Meines Mannes mit Artikel" $ do
        putStrLn "\nTest: 'meines alt Mannes'"
        putStrLn $ "Eingabe: meines alt Mannes"
        putStrLn $ "Erwartet: meines alten Mannes"
        let result = processPhrase "meines alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meines alten Mannes"

      it "Deines Mannes mit Artikel" $ do
        putStrLn "\nTest: 'deines alt Mannes'"
        putStrLn $ "Eingabe: deines alt Mannes"
        putStrLn $ "Erwartet: deines alten Mannes"
        let result = processPhrase "deines alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deines alten Mannes"

      it "Keines Mannes mit Artikel" $ do
        putStrLn "\nTest: 'keines alt Mannes'"
        putStrLn $ "Eingabe: keines alt Mannes"
        putStrLn $ "Erwartet: keines alten Mannes"
        let result = processPhrase "keines alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keines alten Mannes"

      it "Manches Mannes mit Artikel" $ do
        putStrLn "\nTest: 'manches alt Mannes'"
        putStrLn $ "Eingabe: manches alt Mannes"
        putStrLn $ "Erwartet: manches alten Mannes"
        let result = processPhrase "manches alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manches alten Mannes"

      it "Solches Mannes mit Artikel" $ do
        putStrLn "\nTest: 'solches alt Mannes'"
        putStrLn $ "Eingabe: solches alt Mannes"
        putStrLn $ "Erwartet: solches alten Mannes"
        let result = processPhrase "solches alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solches alten Mannes"

      it "Welches Mannes mit Artikel" $ do
        putStrLn "\nTest: 'welches alt Mannes'"
        putStrLn $ "Eingabe: welches alt Mannes"
        putStrLn $ "Erwartet: welches alten Mannes"
        let result = processPhrase "welches alt Mannes" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welches alten Mannes"
      -- G f
      it "Der Animation mit Artikel" $ do
        putStrLn "\nTest: 'der alt Animation'"
        putStrLn $ "Eingabe: der alt Animation"
        putStrLn $ "Erwartet: der alten Animation"
        let result = processPhrase "der alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "der alten Animation"

      it "Einer Animation mit Artikel" $ do
        putStrLn "\nTest: 'einer alt Animation'"
        putStrLn $ "Eingabe: einer alt Animation"
        putStrLn $ "Erwartet: einer alten Animation"
        let result = processPhrase "einer alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "einer alten Animation"

      it "Dieser Animation mit Artikel" $ do
        putStrLn "\nTest: 'dieser alt Animation'"
        putStrLn $ "Eingabe: dieser alt Animation"
        putStrLn $ "Erwartet: dieser alten Animation"
        let result = processPhrase "dieser alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dieser alten Animation"

      it "Jeder Animation mit Artikel" $ do
        putStrLn "\nTest: 'jeder alt Animation'"
        putStrLn $ "Eingabe: jeder alt Animation"
        putStrLn $ "Erwartet: jeder alten Animation"
        let result = processPhrase "jeder alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jeder alten Animation"

      it "Meiner Animation mit Artikel" $ do
        putStrLn "\nTest: 'meiner alt Animation'"
        putStrLn $ "Eingabe: meiner alt Animation"
        putStrLn $ "Erwartet: meiner alten Animation"
        let result = processPhrase "meiner alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meiner alten Animation"

      it "Deiner Animation mit Artikel" $ do
        putStrLn "\nTest: 'deiner alt Animation'"
        putStrLn $ "Eingabe: deiner alt Animation"
        putStrLn $ "Erwartet: deiner alten Animation"
        let result = processPhrase "deiner alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deiner alten Animation"

      it "Keiner Animation mit Artikel" $ do
        putStrLn "\nTest: 'keiner alt Animation'"
        putStrLn $ "Eingabe: keiner alt Animation"
        putStrLn $ "Erwartet: keiner alten Animation"
        let result = processPhrase "keiner alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keiner alten Animation"

      it "Mancher Animation mit Artikel" $ do
        putStrLn "\nTest: 'mancher alt Animation'"
        putStrLn $ "Eingabe: mancher alt Animation"
        putStrLn $ "Erwartet: mancher alten Animation"
        let result = processPhrase "mancher alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "mancher alten Animation"

      it "Solcher Animation mit Artikel" $ do
        putStrLn "\nTest: 'solcher alt Animation'"
        putStrLn $ "Eingabe: solcher alt Animation"
        putStrLn $ "Erwartet: solcher alten Animation"
        let result = processPhrase "solcher alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solcher alten Animation"

      it "Welcher Animation mit Artikel" $ do
        putStrLn "\nTest: 'welcher alt Animation'"
        putStrLn $ "Eingabe: welcher alt Animation"
        putStrLn $ "Erwartet: welcher alten Animation"
        let result = processPhrase "welcher alt Animation" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welcher alten Animation"
      -- G n
      it "Des Bilds mit Artikel" $ do
        putStrLn "\nTest: 'des alt Bilds'"
        putStrLn $ "Eingabe: des alt Bilds"
        putStrLn $ "Erwartet: des alten Bilds"
        let result = processPhrase "des alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "des alten Bilds"

      it "Eines Bilds mit Artikel" $ do
        putStrLn "\nTest: 'eines alt Bilds'"
        putStrLn $ "Eingabe: eines alt Bilds"
        putStrLn $ "Erwartet: eines alten Bilds"
        let result = processPhrase "eines alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "eines alten Bilds"

      it "Dieses Bilds mit Artikel" $ do
        putStrLn "\nTest: 'dieses alt Bilds'"
        putStrLn $ "Eingabe: dieses alt Bilds"
        putStrLn $ "Erwartet: dieses alten Bilds"
        let result = processPhrase "dieses alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "dieses alten Bilds"

      it "Jedes Bilds mit Artikel" $ do
        putStrLn "\nTest: 'jedes alt Bilds'"
        putStrLn $ "Eingabe: jedes alt Bilds"
        putStrLn $ "Erwartet: jedes alten Bilds"
        let result = processPhrase "jedes alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "jedes alten Bilds"

      it "Meines Bilds mit Artikel" $ do
        putStrLn "\nTest: 'meines alt Bilds'"
        putStrLn $ "Eingabe: meines alt Bilds"
        putStrLn $ "Erwartet: meines alten Bilds"
        let result = processPhrase "meines alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "meines alten Bilds"

      it "Deines Bilds mit Artikel" $ do
        putStrLn "\nTest: 'deines alt Bilds'"
        putStrLn $ "Eingabe: deines alt Bilds"
        putStrLn $ "Erwartet: deines alten Bilds"
        let result = processPhrase "deines alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "deines alten Bilds"

      it "Keines Bilds mit Artikel" $ do
        putStrLn "\nTest: 'keines alt Bilds'"
        putStrLn $ "Eingabe: keines alt Bilds"
        putStrLn $ "Erwartet: keines alten Bilds"
        let result = processPhrase "keines alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "keines alten Bilds"

      it "Manches Bilds mit Artikel" $ do
        putStrLn "\nTest: 'manches alt Bilds'"
        putStrLn $ "Eingabe: manches alt Bilds"
        putStrLn $ "Erwartet: manches alten Bilds"
        let result = processPhrase "manches alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "manches alten Bilds"

      it "Solches Bilds mit Artikel" $ do
        putStrLn "\nTest: 'solches alt Bilds'"
        putStrLn $ "Eingabe: solches alt Bilds"
        putStrLn $ "Erwartet: solches alten Bilds"
        let result = processPhrase "solches alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "solches alten Bilds"

      it "Welches Bilds mit Artikel" $ do
        putStrLn "\nTest: 'welches alt Bilds'"
        putStrLn $ "Eingabe: welches alt Bilds"
        putStrLn $ "Erwartet: welches alten Bilds"
        let result = processPhrase "welches alt Bilds" testNouns
        putStrLn $ "Ergebnis: " ++ show result
        result `shouldBe` Right "welches alten Bilds"