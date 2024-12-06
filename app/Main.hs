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
import Prozess
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
                        
                        Right (art, adj', noun,nounStr) -> do
                            let originalNoun = nounStr
                            let nounForm = determineNounForm noun originalNoun art
                            
                            -- Phrase erstellen und verarbeiten
                            let initialPhrase = AdjectivePhrase {
                                article = art,
                                adjective = adj',
                                noun = noun,
                                nounStr = originalNoun,
                                articleType = if art == Nothing then NoArticle else Definite,
                                case_ = Nominative,
                                number = case nounForm of
                                        PluralForm -> Plural
                                        _ -> Singular,
                                nounForm = nounForm 
                            }
                            
                            let phrase = preprocessPhrase initialPhrase
                            let reasoning = getReasoningSteps phrase
                            let ending = getAdjectiveEnding phrase
                            
                            let finalResult = case art of
                                                  Nothing -> adj' ++ ending ++ " " ++  originalNoun
                                                  Just article -> article ++ " " ++ adj' ++ ending ++ " " ++  originalNoun
                            
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