# Bachelorarbeit

## Decentralized Finance: Eine empirische Analyse von Liquidationen auf Aave V3

Dieses Repository enthält den vollständigen Code und die verwendeten Daten zur empirischen Analyse
der Bachelorarbeit.


## Struktur

- `scripts/`  
  R-Skripte zur Datenaufbereitung, Aggregation, Regression und Visualisierung.

- `figures/`  
  Exportierte Abbildungen, die in der Arbeit verwendet werden.

- `data/`  
  On-Chain-Liquidationen und Preisdaten  
  On-Chain-Daten wurden mit einem
  eigenentwickelten Analysewerkzeug extrahiert: https://github.com/RafaBala28/DeFi-Observer  
  

## Methodik

- On-Chain-Liquidationsdaten aus dem Aave-V3-Protokoll
- Deskriptive Statistiken und Konzentrationsmasse (Gini, Lorenz)
- Regressionsanalyse zum Zusammenhang zwischen ETH-Preisbewegungen
  und Liquidationsaktivität

## Reproduzierbarkeit

Alle Ergebnisse können durch Ausführen der Skripte im Ordner `scripts/`
reproduziert werden

## Autor

Rafael Balasteguim da Silva  
Universität Basel
