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

loadNouns :: FilePath -> IO [GermanNoun]
loadNouns filePath = do
  content <- B.readFile filePath
  case eitherDecode content of
    Left err -> error $ "Error parsing JSON: " ++ err
    Right nouns -> return nouns

-- Helper für Quantifikatoren

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
            Right (art, adj', noun, nounStr) -> do
              let originalNoun = nounStr

              -- Erweiterte Debug-Ausgaben für Nomenformen
              putStrLn "\n=== NOUN FORM DEBUG ==="
              putStrLn $ "Base form (word): " ++ T.unpack (word noun)
              putStrLn $ "Genitive form: " ++ T.unpack (genitive noun)
              putStrLn $ "Original noun: " ++ originalNoun

              let nounForm = determineNounForm noun originalNoun art
              putStrLn $ "Determined noun form: " ++ show nounForm

              -- Rest der Debug-Ausgaben
              putStrLn "\n=== PHRASE DEBUG ==="
              putStrLn $ "Original noun: " ++ originalNoun
              putStrLn $ "Noun form detected: " ++ show nounForm

              let initialPhrase =
                    AdjectivePhrase
                      { article = art,
                        adjective = adj',
                        noun = noun,
                        nounStr = originalNoun,
                        articleType = if art == Nothing then NoArticle else Definite,
                        case_ = case nounForm of
                          GenitiveForm -> Genitive
                          _ -> Nominative,
                        number = case nounForm of
                          PluralForm -> Plural
                          _ -> Singular,
                        nounForm = nounForm
                      }

              putStrLn $ "Assigned case: " ++ show (case_ initialPhrase)
              putStrLn $ "Article: " ++ show (article initialPhrase)
              putStrLn "==================\n"

              let phrase = preprocessPhrase initialPhrase
              let reasoning = getReasoningSteps phrase
              let ending = getAdjectiveEnding phrase
              let finalResult = case art of
                    Nothing -> adj' ++ ending ++ " " ++ originalNoun
                    Just article -> article ++ " " ++ adj' ++ ending ++ " " ++ originalNoun

              TIO.putStrLn $ T.pack reasoning
              TIO.putStrLn $ T.pack $ "Final result: " ++ finalResult
              mainLoop nouns

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  nouns <- loadNouns "german_nouns.json"
  mainLoop nouns

{-main Funktion hier um die germannoun.json zu laden und ins nouns zuweisen
danach führen wir das mainloop Funktion aus-}