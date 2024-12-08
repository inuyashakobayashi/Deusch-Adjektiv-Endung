-- |
-- Modul: Validate
-- Beschreibung: Enthält Funktionen zur Klassifizierung deutscher Artikelwörter und Pronomen
-- Dieses Modul dient der Erkennung und Validierung verschiedener Arten von deutschen Artikeln,
-- Pronomen und Determinatoren, die vor Adjektiven stehen können.
module Validate
  ( isAll,
    isDemonstrative,
    isDefinite,
    isNegative,
    isBoth,
    isInterrogative,
    isPossessive,
    isSome,
    isQuantifier,
    isUniversal,
    isIndefinite,
    isSuch,
  )
where

-- |
-- Prüft, ob ein Wort ein Quantifizierer (Mengenwort) ist.
-- Beispiele: "viele Bücher", "wenige Menschen"
isQuantifier :: String -> Bool
isQuantifier word =
  word
    `elem` [ "viel",
             "viele", -- für zählbare und nicht zählbare Mengen
             "wenig",
             "wenige", -- für kleine Mengen
             "einige",
             "mehrere" -- für unbestimmte Mengen
           ]

-- |
-- Prüft, ob ein Wort ein bestimmter Artikel ist.
-- Beispiele: "der Hund", "die Katze", "das Haus"
isDefinite :: String -> Bool
isDefinite word =
  word
    `elem` [ "der",
             "die",
             "das", -- Nominativ
             "dem",
             "den", -- Dativ/Akkusativ
             "des" -- Genitiv
           ]

-- |
-- Prüft, ob ein Wort ein Demonstrativpronomen ist.
-- Beispiele: "dieser Tisch", "jene Blume"
isDemonstrative :: String -> Bool
isDemonstrative word =
  word
    `elem` [ "dieser",
             "diese",
             "dieses", -- dieser-Gruppe
             "diesem",
             "diesen",
             "jener",
             "jene",
             "jenes", -- jener-Gruppe
             "jenem",
             "jenen"
           ]

-- |
-- Prüft, ob ein Wort ein Allquantor ist.
-- Beispiele: "jeder Tag", "jede Woche"
isUniversal :: String -> Bool
isUniversal word =
  word
    `elem` [ "jeder",
             "jede",
             "jedes",
             "jedem",
             "jeden"
           ]

-- |
-- Prüft, ob ein Wort ein unbestimmter Artikel ist.
-- Beispiele: "ein Buch", "eine Tasche"
isIndefinite :: String -> Bool
isIndefinite word =
  word
    `elem` [ "ein",
             "eine",
             "eines", -- Nominativ/Akkusativ
             "einem",
             "einen", -- Dativ/Akkusativ
             "einer" -- Genitiv/Dativ
           ]

-- |
-- Prüft, ob ein Wort ein unbestimmtes Pronomen ist.
-- Beispiele: "manche Menschen", "mancher Tag"
isSome :: String -> Bool
isSome word =
  word
    `elem` [ "mancher",
             "manche",
             "manches",
             "manchem",
             "manchen"
           ]

-- |
-- Prüft, ob ein Wort ein solch-Pronomen ist.
-- Beispiele: "solche Bücher", "solcher Unsinn"
isSuch :: String -> Bool
isSuch word =
  word
    `elem` [ "solcher",
             "solche",
             "solches",
             "solchem",
             "solchen"
           ]

-- |
-- Prüft, ob ein Wort ein Fragepronomen ist.
-- Beispiele: "welches Auto?", "welche Farbe?"
isInterrogative :: String -> Bool
isInterrogative word =
  word
    `elem` [ "welcher",
             "welche",
             "welches",
             "welchem",
             "welchen"
           ]

-- |
-- Prüft, ob ein Wort ein Possessivpronomen ist.
-- Beispiele: "mein Buch", "deine Tasche"
isPossessive :: String -> Bool
isPossessive word =
  word
    `elem` [ "mein",
             "meine",
             "meines", -- 1. Person Singular
             "dein",
             "deine",
             "deines", -- 2. Person Singular
             "sein",
             "seine",
             "seines", -- 3. Person Singular (m/n)
             "ihr",
             "ihre",
             "ihres", -- 3. Person Singular (f)
             "unser",
             "unsere",
             "unseres", -- 1. Person Plural
             "euer",
             "eure",
             "eures" -- 2. Person Plural
           ]

-- |
-- Prüft, ob ein Wort ein Negativartikel ist.
-- Beispiele: "kein Problem", "keine Zeit"
isNegative :: String -> Bool
isNegative word =
  word
    `elem` [ "kein",
             "keine",
             "keines",
             "keinem",
             "keinen",
             "keiner"
           ]

-- |
-- Prüft, ob ein Wort "alle" ist.
-- Beispiel: "alle Menschen"
isAll :: String -> Bool
isAll word =
  word
    `elem` [ "alle"
           ]

-- |
-- Prüft, ob ein Wort "beide" ist.
-- Beispiel: "beide Bücher"
isBoth :: String -> Bool
isBoth word =
  word
    `elem` [ "beide"
           ]