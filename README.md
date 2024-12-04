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
## Container-Build

Alternativ, das Projekt in einem Container laufen zu lassen:

## Mit Podman (Docker)


## Image bauen
```bash
podman build -t wp-app:latest .
```

## Container starten
```bash
podman run -it wp-app:latest
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

## Datenquelle und Lizenz

Die deutschen Wortdaten basieren auf dem UniMorph-Projekt:
- Quelle: [UniMorph DEU Repository](https://github.com/unimorph/deu)
- Lizenz: [CC BY-SA 3.0](https://creativecommons.org/licenses/by-sa/3.0/)

Das Originaldatenset wurde verarbeitet und in ein JSON-Format für dieses Projekt konvertiert. Gemäß der Share-Alike-Bedingungen der CC BY-SA 3.0 Lizenz steht diese abgeleitete Arbeit unter derselben Lizenz.



## Workflow Diagramm

```mermaid
flowchart TD
    A[Start] --> B{Mit Artikel?}
    
    %% Ohne Artikel - der-Form
    B -->|Nein| C{Ist Plural?}
    C -->|Nein| D[Prüfe der-Form]
    D --> D1{Welches Geschlecht?}
    D1 -->|Maskulin| D2[Endung: -er]
    D1 -->|Feminin| D3[Endung: -e]
    D1 -->|Neutral| D4[Endung: -es]
    C -->|Ja| C1[Endung: -e]

    %% Mit Artikel
    B -->|Ja| E{Standard Form?}
    E -->|Nein| F[Endung: -en]
    
    E -->|Ja| G{Ist Plural?}
    G -->|Ja| H[Endung: -en]
    
    G -->|Nein| I{Zeigt Geschlecht?}
    I -->|Ja| J[Endung: -e]
    
    I -->|Nein| K{Welches Geschlecht?}
    K -->|Maskulin| L[Endung: -er]
    K -->|Neutral| M[Endung: -es]

    %% Beispiele
    style O1 fill:#f9f,stroke:#333
    O1[Beispiele:]
    O2[der Mann -> Standard Form]
    O3[die Frau -> Standard Form]
    O4[der Frau -> Nicht Standard Form]
    O5[eine/dieser/jede -> Zeigt Geschlecht]
    O6[sein/mein -> Zeigt kein Geschlecht]