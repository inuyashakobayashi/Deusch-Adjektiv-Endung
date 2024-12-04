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
import Validate
loadNouns :: FilePath -> IO [GermanNoun]
loadNouns filePath = do
    content <- B.readFile filePath
    case eitherDecode content of
        Left err -> error $ "Error parsing JSON: " ++ err
        Right nouns -> return nouns
-- Helper für Quantifikatoren

-- Vorverarbeitung der AdjectivePhrase
preprocessPhrase :: AdjectivePhrase -> AdjectivePhrase
preprocessPhrase phrase =
    case article phrase of
        Just art | isQuantifier art || isAll art || isBoth art ->
            phrase { articleType = NoArticle }
        Just art | isDefinite art ->
            phrase { articleType = Definite }
        Just art | isDemonstrative art ->
            phrase { articleType = Demonstrative }
        Just art | isUniversal art ->
            phrase { articleType = Universal }
        Just art | isIndefinite art ->
            phrase { articleType = Indefinite }
        Just art | isSome art ->
            phrase { articleType = Some }
        Just art | isSuch art ->
            phrase { articleType = Such }
        Just art | isInterrogative art ->
            phrase { articleType = Interrogative }
        Just art | isPossessive art ->
            phrase { articleType = Possessive }
        Just art | isNegative art ->
            phrase { articleType = Negative }
        _ -> phrase
mainLoop :: [GermanNoun] -> IO ()
mainLoop nouns = do
    TIO.putStrLn "\nEnter German phrase (or 'quit' to exit):"
    input <- TIO.getLine
    
    if T.toLower input == T.pack "quit"
        then TIO.putStrLn "Auf Wiedersehen!"
        else do
            -- Erst parseInput versuchen
            case parseInput (T.unpack input) of
                Left parseError -> do
                    TIO.putStrLn $ T.pack parseError  -- Zeigt hilfreichen Tipp
                    mainLoop nouns                    -- Weitermachen
                
                Right result -> do
                    -- Dann validateNoun versuchen
                    case validateNoun nouns result of
                        Left validationError -> do
                            TIO.putStrLn $ T.pack validationError  -- Zeigt Nomen-Vorschläge
                            mainLoop nouns                         -- Weitermachen
                        
                        Right (art, adj', noun) -> do
                            let originalNoun = getOriginalNoun (T.unpack input)
                            let nounIsPlural = case plural noun of
                                    Just pluralForm -> originalNoun == pluralForm
                                    Nothing -> False
                            let isPlural = nounIsPlural
                            
                            -- Phrase erstellen und verarbeiten
                            let initialPhrase = AdjectivePhrase {
                                article = art,
                                adjective = adj',
                                noun = noun,
                                articleType = if art == Nothing then NoArticle else Definite,
                                number = if isPlural then Plural else Singular,
                                case_ = Nominative
                            }
                            
                            let phrase = preprocessPhrase initialPhrase
                            let reasoning = getReasoningSteps phrase
                            let ending = getAdjectiveEnding phrase
                            
                            let finalResult = case art of
                                                  Nothing -> adj' ++ ending ++ " " ++ T.unpack originalNoun
                                                  Just article -> article ++ " " ++ adj' ++ ending ++ " " ++ T.unpack originalNoun
                            
                            -- Ergebnisse anzeigen
                            TIO.putStrLn $ T.pack reasoning
                            TIO.putStrLn $ T.pack $ "Final result: " ++ finalResult
                            mainLoop nouns  -- Weitermachen für nächste Eingabe
main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    nouns <- loadNouns "german_nouns.json"
    mainLoop nouns
{-main Funktion hier um die germannoun.json zu laden und ins nouns zuweisen
danach führen wir das mainloop Funktion aus-}