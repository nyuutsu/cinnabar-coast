# cinnabar-coast

Pokemon Gen 1/2 game data library. Haskell.

## What this is

A Haskell library for working with Gen 1/2 Pokemon game data —
species, moves, learnsets, evolutions, text encoding. Currently
loads and queries static game data; save file parsing is next.

The eventual goal is a CLI tool for reading and editing save files,
with the library doing the work and the CLI as the interface (like
git or ffmpeg — no UI baked in, a GUI would be a separate program).

## Project constraints

- **Types are the design.** If the types are right, functions write
  themselves. Get the types right before writing logic.
- **Respect settled patterns.** Types already in the codebase (like
  `Special` using `Unified|Split`) are design decisions. Don't
  propose alternatives without explicitly flagging the departure.
  Never silently regress to a flatter design when a more precise
  type exists.
- **No UI layer yet.** CLI-only for now. Domain logic must be
  correct before presentation enters the picture.

## Architecture

```
cinnabar-coast/
├── src/Pokemon/
│   ├── Types.hs      -- Core domain types (Species, Move, DVs, GameChar, etc.)
│   ├── Data.hs       -- CSV loader → GameData
│   ├── Stats.hs      -- Exp curves, stat calculation
│   ├── Legality.hs   -- Move classification (how can species learn move?)
│   └── TextCodec.hs  -- Game Boy text encoding/decoding
├── extract/
│   ├── Main.hs       -- Extraction orchestrator (reads pret repos, writes CSVs)
│   └── Extract/
│       ├── ASM.hs    -- Megaparsec primitives for pret ASM format
│       ├── Species.hs     -- base_stats/*.asm → species.csv + tmhm_compat.csv
│       ├── EvosAttacks.hs -- evos_attacks.asm → learnsets.csv + evolutions.csv
│       ├── Moves.hs       -- moves/*.asm → moves.csv
│       ├── TmHm.hs        -- tmhm tables → tmhm.csv
│       ├── EggMoves.hs    -- egg_moves.asm → egg_moves.csv
│       ├── Tutor.hs       -- tutor data → tutor.csv
│       └── Items.hs       -- item constants → items.csv
├── app/Main.hs       -- CLI entry point (demo)
├── data/csv/          -- Game data (extracted from pret by `cabal run extract`)
├── data/charsets/     -- Character encoding JSONs (en, frde, ites, jp × gen1/gen2)
├── data/event-pokemon/ -- Event distribution CSVs (not loaded yet)
├── sketch/            -- Design drafts (not compiled)
└── test/              -- Tests (placeholder)
```

### Key types (in Pokemon.Types)

- `Gen` — Gen1 | Gen2
- `Species` — Pokedex entry with base stats, types, growth rate, gender ratio, egg groups
- `BaseStats` — HP, Attack, Defense, Speed, Special (with gen-aware split)
- `Special` — Unified Int (Gen 1) | Split Int Int (Gen 2)
- `GenderRatio` — AllMale | Female12_5 | Female25 | Female50 | Female75 | AllFemale | Genderless
- `EggGroup` — 15 groups (EggMonster through EggNone). EggNone = legendaries.
- `MoveType` — StandardType PokemonType | CurseType | UnknownType Word8
- `Move` — Move definition (name, MoveType, power, PP)
- `DVs` — 4 determinant values (Attack, Defense, Speed, Special). HP derived.
- `StatExp` — 5 stat experience values. Same storage in both gens.
- `Pokemon` — Instance in a save file, with GenData sum type
- `Machine` — TM Int | HM Int (number only; move mapping is per-gen)
- `LearnMethod` — LevelUp | TMMachine | HMMachine | EggMove | Tradeback | PreEvo | ...
- `LearnSource` — One way a species can learn a move. Nests recursively via
  `sourceVia` to explain compound paths (e.g. PreEvo → Tradeback → TM).
- `EvoTrigger` — Sum type of evolution conditions (level, item, trade, trade-with-item,
  happiness, stat comparison)
- `EvolutionStep` — Edge: species A → species B when trigger fires
- `GameData` — All static data for one gen. Immutable. Pass to functions.
  Includes bidirectional evolution maps (`gameEvolvesInto`, `gameEvolvesFrom`).
- `Language` — English | French | German | Italian | Spanish | Japanese
- `GameChar` — Literal Char | Ligature Text | UnknownByte Word8
- `GameText` — Newtype over [GameChar]. Lossless decoded Game Boy text.
- `TextCodec` — Decode/encode maps for one (Gen, Language) pair.
- `NamingScreen` — One game variant's choosable character set.
- `EventConstraint` — Predicate on Pokemon (Maybe = "any value valid")

### Key design decisions

- Dex number is the universal species key across gens
- DVs and StatExp are gen-agnostic types (same storage format)
- The Special stat split (Gen 1→Gen 2) is modeled as a sum type
  in Special, not as conditional fields
- Empty Maps represent absent GameData features (e.g. no egg moves
  in Gen 1). Species-level fields that are absent in Gen 1 use Maybe
  (e.g. speciesGenderRatio). Both patterns are intentional — use
  whichever fits the data.
- LearnMethod/Tradeback is always relative to the gen you're asking about
- Event profiles are predicates (constraints), not Pokemon instances
- GameText is the lossless intermediate representation for Game Boy text.
  The codec decodes bytes → GameText (preserving ligatures and unknown
  bytes); displayText converts GameText → Text (lossy, for display only).
  This prevents the "normalize everything" trap where information is
  silently lost during conversion.
- NamingScreens are stored per game variant (not unioned) because the
  legality check is existential: "does ANY single screen cover all chars
  in this name?" A union would allow names that no single game can
  actually type — e.g. in German Gen 2, space is choosable in
  Gold/Silver but not Crystal, while × is choosable in Crystal but
  not Gold/Silver. Both characters are in the encoding (other layers
  handle that), but no single game's name entry screen offers both.
- GenderRatio is a sum type (not a raw byte). `genderThreshold` converts
  back to the byte value for stat calculation (not yet integrated).
- EggGroup has EggNone in the enum (not as a Maybe layer) because the ROM
  always stores two slots — the outer Maybe distinguishes Gen 1 (absent)
  from Gen 2 (present).
- CSVs use pret ASM constant names throughout. The extraction layer
  (extract/) handles all format knowledge; the library (Data.hs) does
  pure name→type mapping with no heuristics.
- Trade evolutions are normalized at extraction: EVOLVE_TRADE (plain)
  and EVOLVE_TRADE_ITEM (with held item) are distinct CSV methods.

## Data provenance

CSV files are extracted directly from pret disassemblies (pokered,
pokecrystal) by `cabal run extract -- <pokered-path> <pokecrystal-path>`.
The extract executable parses ASM source with Megaparsec and writes
all 9 CSVs. CSVs use pret ASM constant names throughout (NORMAL,
FIRE, GROWTH_MEDIUM_SLOW, GENDER_F50, EGG_MONSTER, EVOLVE_TRADE_ITEM,
etc.) — Data.hs maps these to domain types.

Event CSVs are curated from distribution records. Charset JSONs are
from Bulbapedia's "Character encoding" and "Text entry" pages
(which derive from pret disassemblies). Files are named by
language group: en, frde, ites, jp.

## What's next (rough order)

1. **Save parser** — binary format reading/writing for Gen 1 and Gen 2
3. **Event matching** — constraint checking against event profiles
4. **CLI interface** — subcommands (info, edit, classify, etc.)
5. Tests alongside each module
6. **Error handling boundary** — extraction and CSV loading use `error`
   because the inputs are controlled (curated CSVs, pret ASM sources).
   The save parser will handle untrusted binary input and should use
   `Either` with a descriptive error type from the start. The `error`
   pattern shouldn't carry over into that work.

## Known tricky areas

- **Evolution chains** (SOLVED): Modeled as flat EvolutionStep edges with
  bidirectional Map lookup. Handles branching (Eevee), babies (Pichu),
  trade, item, happiness, and stat-comparison evolutions. PreEvo
  legality walks backward through gameEvolvesFrom. Three-tier recursion
  architecture prevents infinite loops: classifyMove (full) →
  classifyMoveNoPreEvo (used by PreEvo check) →
  classifyMoveNoTradeback (used by Tradeback check).

- **Gen 1 ↔ Gen 2 species identity**: Gen 1 stores an internal species
  index (not the dex number). Pikachu is dex #25 but internal index
  0x54. Gen 2 uses dex number directly. The parser layer handles
  translation, but the mapping table must be correct and complete.

- **Cross-gen move legality** (SOLVED): "Tradeback" means a move learnable
  in the OTHER gen via trade. It's bidirectional — Gen 1 has TMs that
  Gen 2 doesn't (e.g. Mega Punch), and Gen 2 has egg/tutor moves Gen 1
  doesn't. Implemented in Legality.hs with the three-tier recursion
  (classifyMove → classifyMoveNoPreEvo → classifyMoveNoTradeback).

- **Event profiles are underspecified**: Many event distributions have
  incomplete records (unknown DVs, random OT IDs, hatcher-dependent
  fields). The EventConstraint type uses Maybe to represent this, but
  matching logic needs to handle partial matches gracefully —
  "matches except OT differs" is useful information, not just a
  boolean no.

- **Save file region detection**: Gen 1/2 saves don't explicitly store
  their region/language. Detection relies on heuristics (file size,
  checksum location, known byte patterns). Getting this wrong means
  mangling text. Derive detection logic from pret disassembly sources.

## Haskell style

GHC2024 language edition. `OverloadedStrings` where Text literals
are needed. Strict fields on data types by default. `-Wall` clean,
zero warnings. No orphan instances.

**Be idiomatic.** Use the right combinator when it fits (`fromMaybe`,
`mapMaybe`, `concatMap`, `guard`, `groupBy`, `partition`, `on`).
Don't reimplement what the standard library provides.

**Descriptive names.** Record field prefixes are full words:
`baseAttack` not `bsAtk`, `speciesName` not `specName`,
`gameSpecies` not `gdSpecies`. Stat names spelled out (Attack,
Defense, Speed, Special). Domain abbreviations that ARE the standard
term stay abbreviated: DV, HP, PP, OT, TM, HM. When in doubt,
spell it out.

**No shadowing, no prime-naming.** Don't reuse a binding name in
an inner scope. Don't use `x'` or `xs''` — if two things need
names, find two real names.

**Pattern matching** over if-chains. Guards over nested cases.

**Sugar is good when it's free.** `where` clauses, operator
sections, `<$>`, `<*>` — use them when they make code read better
without hiding meaning.

**Composability.** Small functions that combine well. When a real
pattern emerges, make a clean abstraction — three duplicated blocks
are worse than one clear function.

**New code follows the same rules.** Every naming convention here
applies equally to new code, refactors, and helpers introduced during
changes. If a rename pass cleaned something up, the same patterns
shouldn't come back in through new helpers or variables. `buildRow`
not `mkRow`, `species` not `s`, `collectResults` not `go`.
This includes test code.

**Qualified imports** for containers (`Map`, `Set`, `T` for Text).

**Comments explain WHY, not WHAT.** If a comment restates the code,
delete it.

**Proposals should be elegant** — but we discuss fit with the
project's direction before committing.

**Upgrade existing code** when touching it. Better names, better
combinators, clearer structure.

**Heavy IO goes to Rust** when needed — Rust via FFI for
performance-critical work, pure domain logic stays in Haskell.
This project doesn't use Rust currently but the preference applies
if it ever needs systems-level work.

Derive what's natural for the type. Closed enumerations (`Gen`,
`PokemonType`, `Language`, etc.) should have `Enum, Bounded`.

## Working together

The user is learning Haskell alongside building. Teaching is part
of the work — not separate from it.

- Explain new concepts before using them — combinators, type
  signatures, patterns. Say what they mean and why they work.
  Lecture freely.
- Teach the user to write Haskell, not just watch it appear.
  Explain what to write and why, then have them write it.
- Go slow on structural changes and new abstractions. Discuss
  before committing. No bulk code drops.
- When upgrading code to better patterns, show before and after,
  explain what improved.
- If something isn't clear, stop and explain. Understanding
  matters more than progress.
- Commit messages describe what changed and why, in imperative mood
  ("Add X" rather than "Added X"). Keep subject lines under 72
  characters and put detail in the body when it's needed. Avoid
  em dashes in commit messages.
