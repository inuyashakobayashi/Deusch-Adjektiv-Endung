# Deutsch Adjektiv Endungen

Dieses Tool hilft bei der Bestimmung der korrekten Adjektivendungen in der deutschen Sprache.

## Voraussetzungen

- GHC (Glasgow Haskell Compiler) Version 9.2.8
- Cabal (Build-System und Paketmanager)

## Installation

1. Überprüfen Sie Ihre GHC-Version:
```bash
ghc --version
```

Die Ausgabe sollte sein:
```bash
The Glorious Glasgow Haskell Compilation System, version 9.2.8
```
Falls Sie eine andere Version haben, installieren Sie GHC 9.2.8:
## Windows:

Besuchen Sie https://www.haskell.org/ghcup/
Laden Sie den GHCup Installer herunter und führen Sie ihn aus
Öffnen Sie eine neue Kommandozeile und führen Sie aus:
```bash
ghcup install ghc 9.2.8
ghcup set ghc 9.2.8
```

## Linux/MacOS:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.2.8
ghcup set ghc 9.2.8
```
## Projekt bauen und starten

Navigieren Sie zum Projektverzeichnis (wo sich die wp.cabal Datei befindet)
Bauen Sie das Projekt:
```bash
cabal build
```
Starten Sie das Programm:
```bash
cabal run wp
```
## Verwendung
Nach dem Start können Sie deutsche Phrasen eingeben, um die korrekten Adjektivendungen zu erhalten.
Beispiele für gültige Eingaben:

- "das gut Haus"
- "gut Mann"
- "die schön Frau"
- "ein alt Auto"

Geben Sie die Phrasen ohne Adjektivendungen ein, das Programm wird die korrekten Endungen bestimmen.
#Beenden
Um das Programm zu beenden, drücken Sie Ctrl+C.