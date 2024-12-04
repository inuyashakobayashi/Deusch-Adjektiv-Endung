# Build stage
FROM docker.io/haskell:9.2.8 AS builder

# Setze deutsche Locale für Umlaute
RUN apt-get update && \
    apt-get install -y locales && \
    sed -i -e 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales

ENV LANG de_DE.UTF-8
ENV LC_ALL de_DE.UTF-8

WORKDIR /app

# Kopiere nur die .cabal Datei
COPY *.cabal ./

# Installiere Dependencies
RUN cabal update && \
    cabal build --only-dependencies

# Kopiere den Rest des Projekts und die JSON-Dateien
COPY . .

# Build das Projekt
RUN cabal build && \
    cp $(find dist-newstyle -type f -name wp) /app/wp-executable

# Runtime stage
FROM docker.io/debian:bullseye-slim

# Installiere Runtime Dependencies und deutsche Locale
RUN apt-get update && \
    apt-get install -y libgmp10 locales && \
    sed -i -e 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    rm -rf /var/lib/apt/lists/*

ENV LANG de_DE.UTF-8
ENV LC_ALL de_DE.UTF-8

WORKDIR /app

# Kopiere das Binary UND die JSON-Dateien
COPY --from=builder /app/wp-executable /app/wp
COPY *.json /app/

CMD ["/app/wp"]