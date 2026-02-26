# cinnabar-coast

Pokemon Gen 1/2 save file engine. Haskell. CLI-first architecture.

## What this is

A library + CLI tool that reads, analyzes, and edits Gen 1/2 Pokemon
save files. The engine knows nothing about UI — a GUI is a separate
program that talks to the CLI. This is how git works, how ffmpeg works.

## Project values

- **Types are the design.** If the types are right, functions write
  themselves. If the types are wrong, no code fixes it. Get the types
  right before writing logic.
- **Go slow, talk it through.** The user is learning Haskell alongside
  building this. Every new concept, module, or abstraction should be
  explained and discussed before being committed. No bulk code drops.
- **Do less, not more.** Don't add complexity speculatively. Don't
  abstract things that are only used once. Three similar lines is
  better than a premature abstraction.
- **The user's understanding matters more than progress.** If they
  don't understand something, stop and explain it. Never steamroll
  past confusion for the sake of getting code written.
- **Respect prior decisions.** Types and patterns already in the
  codebase (like Special using Unified|Split) are settled design
  decisions. Don't offer alternatives that contradict them without
  explicitly flagging: "this differs from the existing pattern X,
  here's why." Never silently regress to a flatter/simpler design
  (e.g. offering a 6-tuple or bare Int fields) when a more precise
  type already exists. When presenting options, the option that's
  consistent with existing types should be the default recommendation.
- **No UI layer yet.** The project is deliberately CLI-only for now.
  Don't import brick, don't think about TUI layout, don't design
  for a specific display framework. The domain logic must be
  beautiful and correct before presentation enters the picture.

## Architecture

```
cinnabar-coast/
├── src/Pokemon/
│   ├── Types.hs      -- Core domain types (Species, Move, DVs, GameChar, etc.)
│   ├── Data.hs       -- CSV loader → GameData
│   ├── Stats.hs      -- Exp curves, stat calculation
│   ├── Legality.hs   -- Move classification (how can species learn move?)
│   └── TextCodec.hs  -- Game Boy text encoding/decoding
├── app/Main.hs       -- CLI entry point
├── data/csv/          -- Game data (species, moves, learnsets, TM/HM, evolutions)
├── data/charsets/     -- Character encoding JSONs
├── data/event-pokemon/ -- Event distribution CSVs (not loaded yet)
├── scripts/           -- Data extraction scripts (Python)
├── sketch/            -- Design drafts (not compiled)
└── test/              -- Tests (placeholder)
```

### Key types (in Pokemon.Types)

- `Gen` — Gen1 | Gen2
- `Species` — Pokedex entry with base stats, types, growth rate
- `Move` — Move definition (name, type, power, PP)
- `DVs` — 4 determinant values (Attack, Defense, Speed, Special). HP derived.
- `StatExp` — 5 stat experience values. Same storage in both gens.
- `CalcStats` — Calculated stats from species + DVs + stat exp + level
- `Special` — Unified Int (Gen 1) | Split Int Int (Gen 2)
- `Pokemon` — Instance in a save file, with GenData sum type
- `Machine` — TM Int | HM Int (number only; move mapping is per-gen)
- `LearnMethod` — LevelUp | TMMachine | HMMachine | EggMove | Tradeback | PreEvo | ...
- `LearnSource` — One way a species can learn a move. Nests recursively via
  `sourceVia` to explain compound paths (e.g. PreEvo → Tradeback → TM).
- `EvoTrigger` — Sum type of evolution conditions (level, item, trade, happiness, stat comparison)
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
- Empty Maps represent absent features (no Maybe wrapping needed)
- LearnMethod/Tradeback is always relative to the gen you're asking about
- Event profiles are predicates (constraints), not Pokemon instances
- GameText is the lossless intermediate representation for Game Boy text.
  The codec decodes bytes → GameText (preserving ligatures and unknown
  bytes); displayText converts GameText → Text (lossy, for display only).
  This prevents the "normalize everything" trap where information is
  silently lost during conversion.
- NamingScreens are stored per game variant (not unioned) because the
  legality check is existential: "does ANY single screen cover all chars
  in this name?" A union would miss impossible combinations (e.g. space
  + × in German Gen 2, where Gold has space and Crystal has ×, but no
  single game has both).

## Data provenance

CSV files are from the oldrod project (same author), originally
ripped from pret disassemblies (pokered, pokecrystal). Event CSVs
are curated from distribution records. Charset JSONs define the
Game Boy's custom byte↔Unicode mapping per gen/region.

## What's built

- [x] Core domain types (Pokemon.Types)
- [x] CSV data loader (Pokemon.Data) — loads GameData for Gen 1 and Gen 2
- [x] Stats module (Pokemon.Stats) — exp curves, stat calculation
- [x] Legality module (Pokemon.Legality) — move classification with full
  derivation chains (LevelUp, TM/HM, EggMove, TutorMove, Tradeback, PreEvo)
- [x] Evolution chains — flat edge model with bidirectional lookup maps,
  extracted from pret ASM sources via scripts/generate_evolutions.py
- [x] Text codec (Pokemon.TextCodec) — Game Boy character encoding/decoding
  with GameChar wrapper type, NamingScreen per-variant choosable data

## What's next (rough order)

1. **Fix charset data** — gen1-de.json is wrong (English layout, not FR+DE),
   JP files incomplete (missing 0xE4-0xFF range), gen1-en 0xF0 is yen not
   pokédollar. FR/IT/ES need separate files (differ from EN at 0xBA-0xDF).
2. **Save parser** — binary format reading/writing for Gen 1 and Gen 2
3. **Event matching** — constraint checking against event profiles
4. **CLI interface** — subcommands (info, edit, classify, etc.)
5. Tests alongside each module

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

- **Cross-gen move legality**: "Tradeback" means a move learnable in
  the OTHER gen via trade. It's bidirectional — Gen 1 has TMs that
  Gen 2 doesn't (e.g. Mega Punch), and Gen 2 has egg/tutor moves
  Gen 1 doesn't. The classification function takes a gen parameter
  and checks the other gen as a fallback. This is already designed
  in the MoveCategory type but the implementation needs care.

- **Event profiles are underspecified**: Many event distributions have
  incomplete records (unknown DVs, random OT IDs, hatcher-dependent
  fields). The EventConstraint type uses Maybe to represent this, but
  matching logic needs to handle partial matches gracefully —
  "matches except OT differs" is useful information, not just a
  boolean no.

- **Save file region detection**: Gen 1/2 saves don't explicitly store
  their region/language. Detection relies on heuristics (file size,
  checksum location, known byte patterns). Getting this wrong means
  mangling text. The oldrod code has working detection — study it
  before reimplementing.

## Style guide

- GHC2021 language edition (modern defaults, no extension soup)
- OverloadedStrings where Text literals are needed
- Strict fields (`!`) on data types by default
- Qualified imports for containers (`Map`, `Set`, `T` for Text)
- Pattern matching over if-chains where it reads better
- Comments explain WHY, not WHAT
- No orphan instances
- `-Wall` clean (zero warnings)
- **Naming: be expressive.** Record field prefixes are full words, not
  initials: `baseAttack` not `bsAtk`, `speciesName` not `specName`,
  `gameSpecies` not `gdSpecies`. Stat names are spelled out (Attack,
  Defense, Speed, Special). Domain abbreviations that ARE the standard
  term stay abbreviated: DV, HP, PP, OT, TM, HM. When in doubt,
  spell it out — verbosity is cheap, confusion is expensive.
- Only derive typeclass instances that are actually used. Don't add
  `Enum` or `Bounded` speculatively.

## Haskell notes for the user

The user is learning Haskell. When introducing new concepts:
- Explain what `deriving` clauses do when adding new ones
- Explain `!` (strictness), `$` (application), `.` (composition) on first use
- Show both the type signature and an example when introducing a function
- Don't use point-free style unless it's genuinely clearer
- Prefer explicit pattern matching over fancy combinators for readability

## Relationship to oldrod

oldrod (/home/nyuu/repos/oldrod/) is the predecessor — a Python/Textual
TUI that got 14 commits deep but the UI made the user miserable. The
data files and domain knowledge carry over. The Python code serves as
a reference implementation for binary formats and game mechanics. Don't
import Python patterns wholesale — use Haskell idioms.
