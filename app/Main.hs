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
                    Right (art, adj', noun) -> do
                        let originalNoun = getOriginalNoun (T.unpack input)
                        
                        TIO.putStrLn $ "Debug: Original input noun: " <> originalNoun
                        TIO.putStrLn $ "Debug: Database noun: " <> word noun
                        TIO.putStrLn $ "Debug: Noun gender: " <> gender noun
                        
                        let nounGender = genderFromText $ gender noun
                        let nounIsPlural = lookupNounForPlural originalNoun (word noun) nouns
                        let articleIndicatesPlural = isArticlePlural art nounGender
                        let isPlural = nounIsPlural || articleIndicatesPlural
                        
                        TIO.putStrLn $ "Debug: Actual gender: " <> T.pack (show nounGender)
                        TIO.putStrLn $ "Debug: Noun plural check: " <> T.pack (show nounIsPlural)
                        TIO.putStrLn $ "Debug: Article plural check: " <> T.pack (show articleIndicatesPlural)
                        TIO.putStrLn $ "Debug: Final plural decision: " <> T.pack (show isPlural)
                        
                        let phrase = AdjectivePhrase {
                            article = art,
                            adjective = adj',
                            noun = noun,
                            articleType = if art == Nothing then NoArticle else Definite,
                            possessiveType = Nothing,
                            number = if isPlural then Plural else Singular,
                            case_ = Nominative
                        }
                        let reasoning = getReasoningSteps phrase
                        TIO.putStrLn $ T.pack reasoning
                        let ending = getAdjectiveEnding phrase
                        let finalResult = case art of
                             Nothing -> adj' ++ ending ++ " " ++ T.unpack originalNoun  -- 使用原始名词形式
                             Just article -> article ++ " " ++ adj' ++ ending ++ " " ++ T.unpack originalNoun
                        TIO.putStrLn $ T.pack $ "Final result: " ++ finalResult
            mainLoop nouns
main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    nouns <- loadNouns "germannoun.json"
    mainLoop nouns