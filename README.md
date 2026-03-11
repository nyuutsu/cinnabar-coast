# cinnabar-coast

A Haskell library for working with Pokemon Gen 1 and Gen 2 game data.

Loads species, moves, learnsets, evolutions, text encoding, and event
distribution records from CSV and JSON extracted from the
[pret](https://github.com/pret) disassemblies. Answers structural
questions about the data: what moves can this species learn, how, and
in which gen? Is this moveset legal? Could this Pokemon be from a
known event distribution?

Save file parsing and editing is next.

## Building

Requires GHC and cabal (via [ghcup](https://www.haskell.org/ghcup/)).

```
cabal build
```

## Data

Game data lives in `data/` as CSV and JSON, extracted from the pret
disassemblies (pokered, pokecrystal) by `cabal run extract`.

```
data/
├── csv/               9 CSVs (species, moves, learnsets, evolutions,
│                      TM/HM mappings, TM/HM compatibility, egg moves,
│                      tutor moves, items)
├── charsets/           Text encoding tables per (gen, language) pair
│                      (en, frde, ites, jp × gen1, gen2)
└── event-pokemon/     Event distribution profiles
                       (Japanese, European, PCNY, game-based × gen1, gen2)
```

CSVs use pret ASM constant names throughout. The library maps these to
domain types — no heuristics, no normalization.

## Library modules

| Module | What it does |
|--------|-------------|
| Pokemon.Types | Core domain types (Species, Move, DVs, GameText, LearnSource, ...) |
| Pokemon.Data | CSV/JSON loader → GameData |
| Pokemon.Stats | Exp curves, stat calculation |
| Pokemon.Legality | Move classification — how can a species learn a move? |
| Pokemon.TextCodec | Game Boy text encoding/decoding (lossless round-trip) |

## Re-extracting data

If you have local copies of pokered and pokecrystal:

```
cabal run extract -- path/to/pokered path/to/pokecrystal
```

This parses the ASM source with Megaparsec and writes all 9 CSVs.

## License

BSD-3-Clause
