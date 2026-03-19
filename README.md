# cinnabar-coast

A 👶 nascent 👶 Pokémon Gen 1 & 2 save editor.

Libraryesque: ask structural questions about gen 1 data & get an answer.

* What moves can this species learn, how, and in which gen?

* Is this moveset legal?

* Could this Pokemon be from a known event distribution?

Example move-legality searches are below. We *surface the answer to **why** a move is learnable*.

```md
BLISSEY + SEISMIC_TOSS (Gen 2, L100):                   
    └── Pre-evo (CHANSEY #113))                                                                                     
        └── Tradeback (Gen 1)                                                                                        
            └── TM (TM19)

RAICHU + THUNDER_WAVE (Gen 2, L100):
    ├── Tradeback (Gen 1)
    │   ├── Level-up (L1)
    │   └── TM (TM45)
    ├── Pre-evo (PICHU (#172))
    │   └── Level-up (L8)
    └── Pre-evo (PIKACHU (#25))
        ├── Level-up (L8)
        └── Tradeback (Gen 1)
            ├── Level-up (L9)
            └── TM (TM45)
```

Save file parsing and editing is next.

## Building

Requires GHC and cabal (via [ghcup](https://www.haskell.org/ghcup/)).

```
cabal build
```

## Data

What we track: species, moves, learnsets, evolutions, TM/HM mappings, TM/HM compatibility, egg m oves, tutor moves, items, event distribution profiles, text encoding tables for each gen+language pair

Relevant game data is stored as CSV.

Provided, but, can be generated anew: `cabal run extract -- path/to/pokered path/to/pokecrystal`

CSVs use pret ASM constant names throughout. We map these to domain types.

## License

BSD-3-Clause
