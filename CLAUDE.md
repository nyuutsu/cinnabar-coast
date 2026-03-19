# cinnabar-coast

Pokemon Gen 1/2 save editor. Haskell.

## What this is

A Pokemon Gen 1/2 save editor — CLI tool for reading and editing
save files (like git or ffmpeg — no UI baked in, a GUI would be a
separate program). Domain logic covers species, moves, learnsets,
evolutions, and text encoding. Currently loads and queries static
game data; save file parsing is the active next step.

Core logic lives in the cabal library stanza so the executable and
tests can share code, but there are no external consumers.

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
│   ├── Types.hs           -- Core domain types (Species, Move, DVs, GameChar, etc.)
│   ├── Types/Internal.hs  -- GameChar constructors (Internal module pattern)
│   ├── Data.hs            -- CSV loader → GameData
│   ├── Error.hs           -- LoadError type, renderLoadError, loadOrDie
│   ├── Schema.hs          -- Canonical name↔type maps for pret ASM constants
│   ├── Stats.hs           -- Exp curves, stat calculation
│   ├── Legality.hs        -- Move classification (how can species learn move?)
│   └── TextCodec.hs       -- Game Boy text encoding/decoding
├── extract/
│   ├── Main.hs            -- Extraction orchestrator (reads pret repos, writes CSVs)
│   └── Extract/
│       ├── ASM.hs         -- Megaparsec primitives for pret ASM format
│       ├── Species.hs     -- base_stats/*.asm → species.csv + tmhm_compat.csv
│       ├── EvosAttacks.hs -- evos_attacks.asm → learnsets.csv + evolutions.csv
│       ├── Moves.hs       -- moves/*.asm → moves.csv
│       ├── TMHM.hs        -- tmhm tables → tmhm.csv
│       ├── EggMoves.hs    -- egg_moves.asm → egg_moves.csv
│       └── Items.hs       -- item constants → items.csv
├── app/Main.hs            -- CLI entry point (demo)
├── data/csv/              -- Game data (extracted from pret by `cabal run extract`)
├── data/charsets/         -- Character encoding JSONs (en, frde, ites, jp × gen1/gen2)
├── data/event-pokemon/    -- Event distribution CSVs (not loaded yet)
└── test/                  -- Tests (stats, codec round-trip, move legality)
```

### Key types

See the organized export list in `src/Pokemon/Types.hs` for the full
type catalog. The exports are grouped by section with comments. Below
are only the types whose design isn't obvious from the code alone.

- `Special` — Unified Int (Gen 1) | Split Int Int (Gen 2). Models
  the Special stat split as a sum type rather than conditional fields.
  This pattern recurs throughout the codebase.
- `SpeciesGenFields` — Gen1SpeciesFields | Gen2SpeciesFields. Gen 1
  has no gender, egg groups, or base happiness. Uses a sum type
  mirroring Special (Unified | Split) and GenData (Gen1Data | Gen2Data)
  rather than Maybe fields. Pattern-match on the variant.
- `GameChar` — Opaque outside the codec. Each constructor carries its
  ROM source byte (!Word8), so two GameChars from different bytes are
  distinct even if they display the same glyph. Construction is
  restricted via the Internal module pattern: only Pokemon.TextCodec
  imports Pokemon.Types.Internal; everything else sees the type without
  constructors.
- `GameText` — Newtype over [GameChar]. The lossless intermediate
  representation for Game Boy text. Losslessness is enforced by
  GameChar's embedded byte, not convention. `displayText` converts
  to Text (lossy, for display only).
- `TextCodec` — Decode map only (Map Word8 GameChar). No encode map:
  encoding extracts the byte already embedded in each GameChar.
- `GameData` — All static data for one gen. Four subrecords grouped
  by usage pattern: MachineData (TM/HM mappings + compatibility),
  LearnsetData (level-up, egg, tutor), SpeciesGraph (species +
  bidirectional evolution maps), LookupTables (name→ID, app layer).
  Functions take the subrecord they need, not the whole GameData.
- `LearnSource` — One way a species can learn a move. Nests
  recursively via `sourceVia` to explain compound paths (e.g.
  Tradeback "Gen 1" [TM source] or PreEvo "PIKACHU" [level-up source]).
- `LoadError` — Structured error for the loading boundary (CSV and
  charset). Each constructor carries file path, row number, column
  name, etc. Rendered by `renderLoadError`; `loadOrDie` crashes on
  Left for the current error-or-die style.
- `Row` — CSV row carrying file path and row number provenance
  (lives in Data.hs, not Types.hs). Typed column accessors
  (`intColumn`, `textColumn`, etc.) produce LoadError values with
  full context automatically.

### Key design decisions

- Dex number is the universal species key across gens
- DVs and StatExp are gen-agnostic types (same storage format)
- Empty Maps represent absent GameData features (e.g. no egg moves
  in Gen 1). Species-level fields that are absent in Gen 1 use a
  sum type (SpeciesGenFields), matching the Special and GenData
  pattern. Pattern-match on the gen variant rather than handling Maybe.
- LearnMethod/Tradeback is always relative to the gen you're asking about
- Event profiles are predicates (constraints), not Pokemon instances
- NamingScreens are stored per game variant (not unioned) because the
  legality check is existential: "does ANY single screen cover all chars
  in this name?" A union would allow names that no single game can
  actually type — e.g. in German Gen 2, space is choosable in
  Gold/Silver but not Crystal, while × is choosable in Crystal but
  not Gold/Silver.
- CSVs use pret ASM constant names throughout. The extraction layer
  (extract/) handles all format knowledge; the library (Data.hs) does
  pure name→type mapping with no heuristics.
- Trade evolutions are normalized at extraction: EVOLVE_TRADE (plain)
  and EVOLVE_TRADE_ITEM (with held item) are distinct CSV methods.
- Name-to-type mappings live in Pokemon.Schema as Map Text a values,
  not pattern-match chains. This prevents catch-all clauses from
  silently swallowing new constructors.
- The loading layer uses structured LoadError values returned via
  Either, not error calls. The pure query layer (Legality, Stats)
  is total; errors live at the loading boundary.

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
2. **Event matching** — constraint checking against event profiles
3. **CLI interface** — subcommands (info, edit, classify, etc.)

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
