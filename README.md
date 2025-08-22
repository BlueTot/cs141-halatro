# Halatro

A card game similar to Balatro's Ante 1 written in Haskell, containing an AI that can score on average 744 points per run. The player has 3 plays and 3 discards, and is given a hand of 8 cards that refills after each play/discard. The score is determined by the strength of the hands played.

## Coursework

This was coursework for CS141 Functional Programming, and the assignment scored 88%.

## Installation

First, clone the repository.

```bash
git clone https://github.com/BlueTot/cs141-halatro
```

Run the program using `stack`. Make sure `ghci`, `stack` and related binaries are installed.

```bash#
cd cs141-halatro
stack run
```
