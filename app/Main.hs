module Main where
import Types 
import Utils 
import Parser 
import DeclensionRules
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.IO (hSetEncoding, stdout, stdin, utf8)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B

loadNouns :: FilePath -> IO [GermanNoun]
loadNouns filePath = do
    content <- B.readFile filePath
    case eitherDecode content of
        Left err -> error $ "Error parsing JSON: " ++ err
        Right nouns -> return nouns
-- Helper für Quantifikatoren
isQuantifier :: String -> Bool
isQuantifier word = word `elem` [
    "viel", "viele", 
    "wenig", "wenige", 
    "einige", "mehrere"
    ]
isDefinite :: String -> Bool
isDefinite word = word `elem` [
  "der", "die", "das", 
  "dem", "den",
  "des"
  ]

isDemonstrative :: String -> Bool
isDemonstrative word = word `elem` [
  "dieser", "diese", "dieses",
  "diesem", "diesen",
  "jener", "jene", "jenes",
  "jenem", "jenen"
  ]

isUniversal :: String -> Bool
isUniversal word = word `elem` [
  "jeder", "jede", "jedes",
  "jedem", "jeden"
  ]

isIndefinite :: String -> Bool
isIndefinite word = word `elem` [
  "ein", "eine", "eines",
  "einem", "einen",
  "einer"
  ]

isSome :: String -> Bool
isSome word = word `elem` [
  "mancher", "manche", "manches",
  "manchem", "manchen"
  
  ]

isSuch :: String -> Bool
isSuch word = word `elem` [
  "solcher", "solche", "solches",
  "solchem", "solchen"
  ]

isInterrogative :: String -> Bool
isInterrogative word = word `elem` [
  "welcher", "welche", "welches",
  "welchem", "welchen"
  ]

isPossessive :: String -> Bool
isPossessive word = word `elem` [
  "mein", "meine", "meines",
  "dein", "deine", "deines",
  "sein", "seine", "seines",
  "ihr", "ihre", "ihres",
  "unser", "unsere", "unseres",
  "euer", "eure", "eures"
  ]

isNegative :: String -> Bool
isNegative word = word `elem` [
  "kein", "keine", "keines",
  "keinem", "keinen",
  "keiner"
  ]



isAll :: String -> Bool
isAll word = word `elem` [
  "alle"
  ]

isBoth :: String -> Bool
isBoth word = word `elem` [
  "beide"
  ]
-- Vorverarbeitung der AdjectivePhrase
preprocessPhrase :: AdjectivePhrase -> AdjectivePhrase
preprocessPhrase phrase =
    case article phrase of
        Just art | isQuantifier art || isAll art || isBoth art -> 
            phrase { articleType = NoArticle }
        _ -> phrase

-- Modifizierte mainLoop
mainLoop :: [GermanNoun] -> IO ()
mainLoop nouns = do
    TIO.putStrLn "\nEnter German phrase (or 'quit' to exit):"
    input <- TIO.getLine
    if T.toLower input == T.pack "quit"
        then TIO.putStrLn "Auf Wiedersehen!"
        else do
            case parseInput (T.unpack input) of
                Left err -> do
                    TIO.putStrLn $ T.pack $ "Error: " ++ err
                Right result -> case validateNoun nouns result of
                    Left err -> TIO.putStrLn $ T.pack err
                    Right (art, adj', noun) -> do--hier noun ist ein Objekt
                        let originalNoun = getOriginalNoun (T.unpack input)
                        let nounIsPlural = case plural noun of
                                                Just pluralForm -> originalNoun == pluralForm
                                                Nothing -> False
                        let isPlural = nounIsPlural
                        
                        -- Erstelle ursprüngliche Phrase
                        let initialPhrase = AdjectivePhrase {
                            article = art,
                            adjective = adj',
                            noun = noun,
                            articleType = if art == Nothing then NoArticle else Definite,--hier sollte besser sein,kann man ein Atikelcheck funktion Schreiben
                            number = if isPlural then Plural else Singular,
                            case_ = Nominative --hier kann man auch checken
                        }
                        
                        -- Vorverarbeitung der Phrase für Quantifikatoren
                        let phrase = preprocessPhrase initialPhrase
                        
                        -- Verwende die unveränderte getReasoningSteps Funktion
                        let reasoning = getReasoningSteps phrase
                        let ending = getAdjectiveEnding phrase
                        
                        -- Rest der Logik bleibt gleich
                        let finalResult = case art of
                                              Nothing      -> adj' ++ ending ++ " " ++ T.unpack originalNoun
                                              Just article -> article ++ " " ++ adj' ++ ending ++ " " ++ T.unpack originalNoun
                        
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