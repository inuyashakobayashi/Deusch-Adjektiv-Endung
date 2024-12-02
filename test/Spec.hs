{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Types
import Utils
import Parser
import DeclensionRules
import qualified Data.Text as T

processPhrase :: String -> [GermanNoun] -> Either String String
processPhrase input nouns = do
    parsed <- parseInput input
    (art, adj, noun) <- validateNoun nouns parsed
    let phrase = AdjectivePhrase {
            article = art,
            adjective = adj,
            noun = noun,
            articleType = if art == Nothing then NoArticle else Definite,
            possessiveType = Nothing,
            number = Singular,
            case_ = Nominative
        }
    let ending = getAdjectiveEnding phrase
    return $ case art of
        Nothing -> adj ++ ending ++ " " ++ T.unpack (word noun)
        Just article -> article ++ " " ++ adj ++ ending ++ " " ++ T.unpack (word noun)

main :: IO ()
main = hspec $ do
    let testNouns = 
            [ GermanNoun 
                { word = T.pack "Bruder"
                , gender = T.pack "m"
                , plural = Just "Brüder"
                }
            , GermanNoun 
                { word = T.pack "Homie"
                , gender = T.pack "m"
                , plural = Just "Homies"
                }
            , GermanNoun 
                { word = T.pack "Digga"
                , gender = T.pack "m"
                , plural = Just "Diggas"
                }
            , GermanNoun 
                { word = T.pack "Schwester"
                , gender = T.pack "f"
                , plural = Just "Schwestern"
                }
            , GermanNoun 
                { word = T.pack "Auto"
                , gender = T.pack "n"
                , plural = Just "Autos"
                }
            ]
    
    describe "Krasse Adjektivendungen Tests" $ do
        it "Sein Homie" $ do
            putStrLn "\nTest: 'sein cool Homie'"
            putStrLn $ "Eingabe: sein cool Homie"
            putStrLn $ "Erwartet: sein cooler Homie"
            let result = processPhrase "sein cool Homie" testNouns
            putStrLn $ "Ergebnis: " ++ show result
            result `shouldBe` Right "sein cooler Homie"
        
        it "Ihre Schwester" $ do
            putStrLn "\nTest: 'ihre crazy Schwester'"
            putStrLn $ "Eingabe: ihre crazy Schwester"
            putStrLn $ "Erwartet: ihre crazy Schwester"
            let result = processPhrase "ihre crazy Schwester" testNouns
            putStrLn $ "Ergebnis: " ++ show result
            result `shouldBe` Right "ihre crazy Schwester"
        
        it "Mein Digga" $ do
            putStrLn "\nTest: 'mein wild Digga'"
            putStrLn $ "Eingabe: mein wild Digga"
            putStrLn $ "Erwartet: mein wilder Digga"
            let result = processPhrase "mein wild Digga" testNouns
            putStrLn $ "Ergebnis: " ++ show result
            result `shouldBe` Right "mein wilder Digga"
        
        it "Jeder Bruder" $ do
            putStrLn "\nTest: 'jeder fresh Bruder'"
            putStrLn $ "Eingabe: jeder fresh Bruder"
            putStrLn $ "Erwartet: jeder freshe Bruder"
            let result = processPhrase "jeder fresh Bruder" testNouns
            putStrLn $ "Ergebnis: " ++ show result
            result `shouldBe` Right "jeder freshe Bruder"
        
        it "Dein Auto" $ do
            putStrLn "\nTest: 'dein lit Auto'"
            putStrLn $ "Eingabe: dein lit Auto"
            putStrLn $ "Erwartet: dein lites Auto"
            let result = processPhrase "dein lit Auto" testNouns
            putStrLn $ "Ergebnis: " ++ show result
            result `shouldBe` Right "dein lites Auto"
        
        it "Unser Homie" $ do
            putStrLn "\nTest: 'unser real Homie'"
            putStrLn $ "Eingabe: unser real Homie"
            putStrLn $ "Erwartet: unser realer Homie"
            let result = processPhrase "unser real Homie" testNouns
            putStrLn $ "Ergebnis: " ++ show result
            result `shouldBe` Right "unser realer Homie"
        
        it "Dieser Digga" $ do
            putStrLn "\nTest: 'dieser chill Digga'"
            putStrLn $ "Eingabe: dieser chill Digga"
            putStrLn $ "Erwartet: dieser chille Digga"
            let result = processPhrase "dieser chill Digga" testNouns
            putStrLn $ "Ergebnis: " ++ show result
            result `shouldBe` Right "dieser chille Digga"

        it "Ihre Schwester ohne Artikel" $ do
            putStrLn "\nTest: 'krass Schwester'"
            putStrLn $ "Eingabe: krass Schwester"
            putStrLn $ "Erwartet: krasse Schwester"
            let result = processPhrase "krass Schwester" testNouns
            putStrLn $ "Ergebnis: " ++ show result
            result `shouldBe` Right "krasse Schwester"