-- | Core domain types for cinnabar-coast.
--
-- These model the Pokémon Gen 1/2 domain. They are pure data definitions:
-- no IO, no parsing, no formatting. Every value is immutable.
--
-- Design rule: types model the DOMAIN, not the binary format. The save
-- file stores a Gen 1 species as an internal index byte; we store the
-- dex number because that's what the domain cares about. The parser
-- layer handles translation.
--
-- Open questions are marked with [?] for discussion.

module Pokemon.Types where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word8, Word16)
import Data.Bits ((.&.), (.|.), shiftL)


-- ── Generation ──────────────────────────────────────────────────

data Gen = Gen1 | Gen2
  deriving (Eq, Ord, Show, Enum, Bounded)


-- ── Pokémon Type (element, not Haskell type) ────────────────────

data PokemonType
  = Normal | Fighting | Flying | Poison | Ground | Rock
  | Bug | Ghost | Steel
  | Fire | Water | Grass | Electric | Psychic | Ice | Dragon | Dark
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Steel and Dark don't exist in Gen 1. We don't enforce that here —
-- it's a data invariant, not a type-level one. A Gen 1 species will
-- simply never have type Steel or Dark in the CSV.
--
-- [?] The Game Boy uses bizarre internal type IDs (0x00–0x1B with
-- gaps). We could store those or map to this clean enum. Mapping is
-- better for the domain; the raw IDs live in the parser layer.


-- ── Growth Rate ─────────────────────────────────────────────────

data GrowthRate
  = MediumFast    -- n³
  | SlightlyFast  -- ¾n³ + 10n² − 30    (a.k.a. "Erratic" in some refs)
  | SlightlySlow  -- ¾n³ + 20n² − 70    (a.k.a. "Fluctuating")
  | MediumSlow    -- ⁶⁄₅n³ − 15n² + 100n − 140
  | Fast          -- ⅘n³
  | Slow          -- ⁵⁄₄n³
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Each species has exactly one growth rate. The formula maps
-- (GrowthRate, Level) → Exp and the inverse (GrowthRate, Exp) → Level.
-- These are pure functions, defined elsewhere.


-- ── Base Stats ──────────────────────────────────────────────────

-- | Base stats from the Pokédex definition. Every species has these.
data BaseStats = BaseStats
  { bsHP  :: !Int
  , bsAtk :: !Int
  , bsDef :: !Int
  , bsSpd :: !Int
  , bsSpc :: !Special
  } deriving (Eq, Show)

-- | The Special stat is ONE value in Gen 1, TWO in Gen 2.
-- This is a property of the species definition, not the individual
-- Pokémon — a Pikachu in Gen 1 has Spc 50; in Gen 2 it has
-- SpAtk 50 / SpDef 40.
data Special
  = Unified !Int          -- Gen 1: one Special
  | Split   !Int !Int     -- Gen 2: (SpAtk, SpDef)
  deriving (Eq, Show)

-- | Extract SpAtk, regardless of gen. Unified → same value for both.
spcAtk :: Special -> Int
spcAtk (Unified s)  = s
spcAtk (Split sa _) = sa

-- | Extract SpDef, regardless of gen.
spcDef :: Special -> Int
spcDef (Unified s)  = s
spcDef (Split _ sd) = sd


-- ── Species (Pokédex entry) ─────────────────────────────────────

-- | A species definition. Static game data — never changes at runtime.
data Species = Species
  { specDex        :: !Int
  , specName       :: !Text
  , specBaseStats  :: !BaseStats
  , specTypes      :: !(PokemonType, PokemonType)
  , specCatchRate  :: !Int
  , specGrowthRate :: !GrowthRate
  -- Gen 2 fields. For Gen 1 entries these are zero/default.
  -- [?] Should we make these Maybe instead? Pro: honest about absence.
  -- Con: every Gen 2 consumer has to unwrap. Since we always HAVE a
  -- value for Gen 2 species (and Gen 1 consumers never look at them),
  -- bare values with "0 = not applicable" might be fine.
  , specGenderRatio   :: !Int    -- 0=all♂, 254=all♀, 255=genderless
  , specEggGroups     :: !(Int, Int)
  , specBaseHappiness :: !Int
  } deriving (Eq, Show)


-- ── Move ────────────────────────────────────────────────────────

data Move = Move
  { moveId       :: !Int
  , moveName     :: !Text
  , moveType     :: !PokemonType
  , movePower    :: !Int    -- 0 for status moves
  , moveAccuracy :: !Int    -- 0 for never-miss moves
  , movePP       :: !Int    -- base PP (before PP Ups)
  } deriving (Eq, Show)


-- ── DVs ─────────────────────────────────────────────────────────

-- | Determinant Values. The same 4 values in both gens.
--
-- CRITICAL INSIGHT: DVs don't split with the Special stat. A Gen 2
-- Pokémon still has one dvSpc that governs BOTH SpAtk and SpDef.
-- The split only affects base stats and the stat calc formula.
-- This means transplanting a Pokémon between gens preserves DVs
-- perfectly — no data is lost or gained.
data DVs = DVs
  { dvAtk :: !Int    -- 0–15
  , dvDef :: !Int    -- 0–15
  , dvSpd :: !Int    -- 0–15
  , dvSpc :: !Int    -- 0–15  (both SpAtk AND SpDef in Gen 2)
  } deriving (Eq, Show)

-- | HP DV is never stored — always derived from the low bits.
dvHP :: DVs -> Int
dvHP dv =
      (dvAtk dv .&. 1) `shiftL` 3
  .|. (dvDef dv .&. 1) `shiftL` 2
  .|. (dvSpd dv .&. 1) `shiftL` 1
  .|. (dvSpc dv .&. 1)

-- | Max DVs (all 15). Common operation for editors.
maxDVs :: DVs
maxDVs = DVs 15 15 15 15

-- | Shiny check (Gen 2 only): Def=10, Spd=10, Spc=10, Atk ∈ {2,3,6,7,10,11,14,15}
isShiny :: DVs -> Bool
isShiny dv =
  dvDef dv == 10 && dvSpd dv == 10 && dvSpc dv == 10 && (dvAtk dv .&. 2) /= 0


-- ── Stat Exp ────────────────────────────────────────────────────

-- | Stat Experience ("EVs" in later gens, but the mechanic is different).
--
-- Same 5 values in both gens. Like DVs, seSpc is ONE value that
-- applies to both SpAtk and SpDef calculations in Gen 2. Cross-gen
-- transfer preserves all stat exp perfectly.
data StatExp = StatExp
  { seHP  :: !Int    -- 0–65535
  , seAtk :: !Int
  , seDef :: !Int
  , seSpd :: !Int
  , seSpc :: !Int    -- single value, even in Gen 2
  } deriving (Eq, Show)

zeroStatExp :: StatExp
zeroStatExp = StatExp 0 0 0 0 0

maxStatExp :: StatExp
maxStatExp = StatExp 65535 65535 65535 65535 65535


-- ── Move Slot ───────────────────────────────────────────────────

-- | One of four move slots. Groups the move with its PP state.
--
-- In the binary format, moves and PPs are stored separately (4 move
-- bytes, then later 4 PP bytes). But in the domain, a slot is the
-- natural unit: "slot 2 has Thunderbolt with 2 PP Ups and 24/24 PP."
--
-- [?] Should empty slots be Maybe MoveSlot, or MoveSlot with moveId=0?
-- Maybe is more honest. moveId=0 is what the binary format uses.
-- Let's use Maybe — it forces callers to handle the empty case.
data MoveSlot = MoveSlot
  { msMove      :: !Int    -- move ID (never 0 in a Just)
  , msPPUps     :: !Int    -- 0–3
  , msCurrentPP :: !Int    -- 0–63
  } deriving (Eq, Show)

type MoveSlots = (Maybe MoveSlot, Maybe MoveSlot, Maybe MoveSlot, Maybe MoveSlot)


-- ── Pokemon (instance in a save file) ───────────────────────────

-- | A Pokémon as it exists in a save file.
--
-- The core fields are identical across gens — same bytes, same
-- meaning, same ranges. The gen-specific fields live in GenData.
data Pokemon = Pokemon
  { pokDex      :: !Int
  , pokNickname :: !Text
  , pokOTName   :: !Text
  , pokOTID     :: !Int          -- 0–65535
  , pokLevel    :: !Int          -- 1–100
  , pokExp      :: !Int          -- 0–16,777,215 (24-bit)
  , pokMoves    :: !MoveSlots
  , pokDVs      :: !DVs
  , pokStatExp  :: !StatExp
  , pokStatus   :: !Word8        -- condition flags (poisoned, etc.)
  , pokGenData  :: !GenData
  } deriving (Eq, Show)

-- | Fields that differ between Gen 1 and Gen 2 save formats.
--
-- Gen 1 stores types and catch rate per-Pokémon (redundant with
-- species data, but that's the binary format).
-- Gen 2 adds held items, friendship, Pokérus, and catch metadata.
data GenData
  = G1Data
      { g1SpecIndex :: !Word8       -- Gen 1 internal species index
      , g1Type1     :: !PokemonType
      , g1Type2     :: !PokemonType
      , g1CatchRate :: !Word8
      }
  | G2Data
      { g2HeldItem   :: !Word8
      , g2Friendship :: !Word8      -- 0–255
      , g2Pokerus    :: !Word8      -- upper nibble=strain, lower=days
      , g2CaughtData :: !Word16     -- packed: time, location, OT gender, level
      }
  deriving (Eq, Show)


-- ── Move Classification ─────────────────────────────────────────

-- | How a species can learn a move.
data MoveCategory
  = LevelUp       -- learned by leveling up
  | TMMachine     -- taught by TM        [?] "TM" clashes with Haskell module prefix convention
  | HMMachine     -- taught by HM
  | EggMove       -- inherited via breeding (Gen 2 only)
  | TutorMove     -- move tutor (Crystal only)
  | Tradeback     -- learnable only in the other gen via trade
  | EventMove     -- event distribution exclusive
  | PreEvo        -- learnable by a pre-evolved form, not this species
  | UnknownSource -- no known legal source
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A classification result: one tag describing one way to learn a move.
-- A move can have MULTIPLE tags (e.g. learnable by both level-up AND TM).
data MoveTag = MoveTag
  { mtCategory :: !MoveCategory
  , mtLabel    :: !Text           -- "L21", "TM38", "HM04", "EGG", etc.
  } deriving (Eq, Show)


-- ── TM / HM ────────────────────────────────────────────────────

-- | A teaching machine. The NUMBER is stable across gens (TM01 is
-- always TM01) but the MOVE it teaches differs. TM01 is Mega Punch
-- in Gen 1 and DynamicPunch in Gen 2.
--
-- This means the mapping (Machine → Move) is per-gen data, stored
-- in GameData. The Machine type just identifies which machine.
data Machine
  = TM !Int     -- TM01–TM50
  | HM !Int     -- HM01–HM07 (Gen 1) or HM01–HM08 (Gen 2)
  deriving (Eq, Ord, Show)


-- ── Game Data (loaded once per gen) ─────────────────────────────

-- | All static game data for one generation. Loaded from CSVs once,
-- then queried by pure functions.
--
-- This is the single entry point: you get a GameData, then ask it
-- questions. No global state, no singletons.
data GameData = GameData
  { gdGen           :: !Gen
  , gdSpecies       :: !(Map Int Species)        -- dex → Species
  , gdMoves         :: !(Map Int Move)           -- move ID → Move
  , gdMachines      :: !(Map Machine Int)        -- Machine → move ID
  , gdMachineCompat :: !(Map Int (Set Machine))  -- dex → {machines it can learn}
  , gdLevelUp       :: !(Map Int [(Int, Int)])    -- dex → [(level, move_id)]
  , gdEggMoves      :: !(Map Int (Set Int))       -- dex → {move_ids}
  , gdTutorMoves    :: !(Map Int (Set Int))       -- dex → {move_ids}
  , gdItems         :: !(Map Int Text)            -- item ID → name
  } deriving (Show)

-- Empty maps for Gen 1's absent features:
-- gdEggMoves, gdTutorMoves, gdItems will all be Data.Map.empty.
-- No Maybe wrapping needed — an empty map IS "this feature doesn't exist."


-- ── Event Constraints ───────────────────────────────────────────

-- | An event distribution profile. This is NOT a Pokémon — it's a
-- PREDICATE on Pokémon. Each field is Maybe because event profiles
-- are intentionally underspecified:
--
--   Nothing = "any value is valid" (unknown, random, or hatcher's)
--   Just x  = "must be exactly x"
--
-- Matching a Pokémon against an EventConstraint is just: do all the
-- Just fields match? This replaces oldrod's boolean flags
-- (ot_id_random, ot_hatcher) with a uniform representation.
data EventConstraint = EventConstraint
  { ecName     :: !Text             -- distribution name (always known)
  , ecDex      :: !Int              -- species (always known)
  , ecGen      :: !Gen              -- generation (always known)
  , ecLevel    :: !(Maybe Int)
  , ecMoves    :: ![Maybe Int]      -- up to 4; Nothing = any in that slot
  , ecOTName   :: !(Maybe Text)     -- Nothing = hatcher's or unknown
  , ecOTID     :: !(Maybe Int)      -- Nothing = random or unknown
  , ecHeldItem :: !(Maybe Int)      -- Nothing = any / not applicable
  , ecDVs      :: !DVConstraint
  , ecShiny    :: !ShinyStatus
  , ecGender   :: !GenderStatus
  , ecGames    :: ![Text]           -- which games ("R","B","Y","G","S","C")
  } deriving (Eq, Show)

-- | How much we know about an event's DVs.
data DVConstraint
  = AnyDVs                          -- most events: DVs are random
  | ExactDVs DVs                    -- rare: specific spread documented
  | PartialDVs                      -- some values known, others random
      (Maybe Int)                   --   Atk
      (Maybe Int)                   --   Def
      (Maybe Int)                   --   Spd
      (Maybe Int)                   --   Spc
  deriving (Eq, Show)

data ShinyStatus
  = ShinyAlways | ShinyPossible | ShinyNever | ShinyUnknown
  deriving (Eq, Show)

data GenderStatus
  = MaleOnly | FemaleOnly | EitherGender | GenderUnknown
  deriving (Eq, Show)


-- ── Match Result ────────────────────────────────────────────────

-- | Result of checking a Pokémon against event constraints.
--
-- [?] This could also carry information about WHICH fields matched
-- and which didn't, for UI display ("matches Mew event except OT
-- name differs"). That might be a list of field-level results
-- rather than a flat enum. Think about this before implementing.
data EventMatch
  = ExactMatch EventConstraint      -- all specified fields match
  | PartialMatch EventConstraint    -- species + some fields match
      [Text]                        -- which fields diverged
  | NoMatch
  deriving (Eq, Show)
