-- | Core domain types for cinnabar-coast.
--
-- Pure data definitions: no IO, no parsing, no formatting.
-- Every value is immutable. Types model the domain, not the
-- binary save format — the parser layer handles translation.

module Pokemon.Types
  ( -- * Generation
    Gen (..)

    -- * Pokemon Types
  , PokemonType (..)

    -- * Growth Rate
  , GrowthRate (..)

    -- * Species (static game data)
  , Species (..)
  , BaseStats (..)
  , SpecialBase (..)
  , spcAtk, spcDef

    -- * Moves (static game data)
  , Move (..)
  , MoveSlot (..)
  , MoveSlots

    -- * DVs and Stat Exp
  , DVs (..)
  , dvHP, maxDVs, isShiny
  , StatExp (..)
  , zeroStatExp, maxStatExp

    -- * Pokemon (save file instance)
  , Pokemon (..)
  , GenData (..)

    -- * TM / HM
  , Machine (..)

    -- * Move classification
  , MoveCategory (..)
  , MoveTag (..)

    -- * Game data (loaded per generation)
  , GameData (..)

    -- * Event constraints
  , EventConstraint (..)
  , DVConstraint (..)
  , ShinyStatus (..)
  , GenderStatus (..)
  , EventMatch (..)
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word8, Word16)


-- ── Generation ──────────────────────────────────────────────────

data Gen = Gen1 | Gen2
  deriving (Eq, Ord, Show, Enum, Bounded)


-- ── Pokémon Type ────────────────────────────────────────────────

-- | Element type. Steel and Dark don't exist in Gen 1 — that's a
-- data invariant, not enforced at the type level.
data PokemonType
  = Normal | Fighting | Flying | Poison | Ground | Rock
  | Bug | Ghost | Steel
  | Fire | Water | Grass | Electric | Psychic | Ice | Dragon | Dark
  deriving (Eq, Ord, Show, Enum, Bounded)


-- ── Growth Rate ─────────────────────────────────────────────────

data GrowthRate
  = MediumFast    -- n³
  | SlightlyFast  -- ¾n³ + 10n² − 30
  | SlightlySlow  -- ¾n³ + 20n² − 70
  | MediumSlow    -- ⁶⁄₅n³ − 15n² + 100n − 140
  | Fast          -- ⅘n³
  | Slow          -- ⁵⁄₄n³
  deriving (Eq, Ord, Show, Enum, Bounded)


-- ── Base Stats ──────────────────────────────────────────────────

data BaseStats = BaseStats
  { bsHP  :: !Int
  , bsAtk :: !Int
  , bsDef :: !Int
  , bsSpd :: !Int
  , bsSpc :: !SpecialBase
  } deriving (Eq, Show)

-- | Gen 1 has one Special stat; Gen 2 splits it into SpAtk / SpDef.
data SpecialBase
  = Unified !Int          -- Gen 1
  | Split   !Int !Int     -- Gen 2: (SpAtk, SpDef)
  deriving (Eq, Show)

spcAtk :: SpecialBase -> Int
spcAtk (Unified s)  = s
spcAtk (Split sa _) = sa

spcDef :: SpecialBase -> Int
spcDef (Unified s)  = s
spcDef (Split _ sd) = sd


-- ── Species ─────────────────────────────────────────────────────

data Species = Species
  { specDex           :: !Int
  , specName          :: !Text
  , specBaseStats     :: !BaseStats
  , specTypes         :: !(PokemonType, PokemonType)
  , specCatchRate     :: !Int
  , specGrowthRate    :: !GrowthRate
  -- Gen 2 fields. Nothing for Gen 1 entries.
  , specGenderRatio   :: !(Maybe Int)  -- 0=all♂, 254=all♀, 255=genderless
  , specEggGroups     :: !(Maybe (Int, Int))
  , specBaseHappiness :: !(Maybe Int)
  } deriving (Eq, Show)


-- ── Move ────────────────────────────────────────────────────────

data Move = Move
  { moveId       :: !Int
  , moveName     :: !Text
  , moveType     :: !PokemonType
  , movePower    :: !Int    -- 0 for status moves
  , moveAccuracy :: !Int    -- 0 for never-miss
  , movePP       :: !Int    -- base PP before PP Ups
  } deriving (Eq, Show)


-- ── DVs ─────────────────────────────────────────────────────────

-- | Determinant Values. Same 4 values in both gens.
-- dvSpc governs both SpAtk and SpDef in Gen 2 — the split only
-- affects base stats and stat calculation, not DV storage.
data DVs = DVs
  { dvAtk :: !Int    -- 0–15
  , dvDef :: !Int
  , dvSpd :: !Int
  , dvSpc :: !Int    -- SpAtk AND SpDef in Gen 2
  } deriving (Eq, Show)

-- | HP DV: derived from the low bits of the other four.
dvHP :: DVs -> Int
dvHP dv =
      (dvAtk dv .&. 1) `shiftL` 3
  .|. (dvDef dv .&. 1) `shiftL` 2
  .|. (dvSpd dv .&. 1) `shiftL` 1
  .|. (dvSpc dv .&. 1)

maxDVs :: DVs
maxDVs = DVs 15 15 15 15

-- | Shiny (Gen 2): Def=10, Spd=10, Spc=10, Atk bit 1 set.
isShiny :: DVs -> Bool
isShiny dv =
  dvDef dv == 10 && dvSpd dv == 10 && dvSpc dv == 10
  && (dvAtk dv .&. 2) /= 0


-- ── Stat Exp ────────────────────────────────────────────────────

-- | Stat Experience. Same 5 values in both gens.
-- seSpc applies to both SpAtk and SpDef calcs in Gen 2.
data StatExp = StatExp
  { seHP  :: !Int    -- 0–65535
  , seAtk :: !Int
  , seDef :: !Int
  , seSpd :: !Int
  , seSpc :: !Int
  } deriving (Eq, Show)

zeroStatExp :: StatExp
zeroStatExp = StatExp 0 0 0 0 0

maxStatExp :: StatExp
maxStatExp = StatExp 65535 65535 65535 65535 65535


-- ── Move Slot ───────────────────────────────────────────────────

-- | One of four move slots. Nothing = empty slot.
-- msCurrentPP is remaining uses (numerator). Max PP is derived:
--   max_pp = base_pp + (base_pp * msPPUps / 5)
data MoveSlot = MoveSlot
  { msMove      :: !Int    -- move ID
  , msPPUps     :: !Int    -- 0–3
  , msCurrentPP :: !Int    -- 0–63, remaining uses
  } deriving (Eq, Show)

type MoveSlots = (Maybe MoveSlot, Maybe MoveSlot, Maybe MoveSlot, Maybe MoveSlot)


-- ── Pokemon ─────────────────────────────────────────────────────

data Pokemon = Pokemon
  { pokDex      :: !Int
  , pokNickname :: !Text
  , pokOTName   :: !Text
  , pokOTID     :: !Int       -- 0–65535
  , pokLevel    :: !Int       -- 1–100
  , pokExp      :: !Int       -- 0–16,777,215
  , pokMoves    :: !MoveSlots
  , pokDVs      :: !DVs
  , pokStatExp  :: !StatExp
  , pokStatus   :: !Word8
  , pokGenData  :: !GenData
  } deriving (Eq, Show)

-- | Gen-specific fields. Pattern match to handle each gen.
data GenData
  = G1Data
      { g1SpecIndex :: !Word8
      , g1Type1     :: !PokemonType
      , g1Type2     :: !PokemonType
      , g1CatchRate :: !Word8
      }
  | G2Data
      { g2HeldItem   :: !Word8
      , g2Friendship :: !Word8
      , g2Pokerus    :: !Word8
      , g2CaughtData :: !Word16
      }
  deriving (Eq, Show)


-- ── TM / HM ────────────────────────────────────────────────────

-- | A teaching machine. The number is stable within a gen;
-- the move it teaches is per-gen data in GameData.
-- Gen 1: TM01–50, HM01–05. Gen 2: TM01–50, HM01–07.
data Machine
  = TM !Int
  | HM !Int
  deriving (Eq, Ord, Show)


-- ── Move Classification ─────────────────────────────────────────

-- | How a species can learn a move, relative to a specific gen.
data MoveCategory
  = LevelUp
  | TMMachine
  | HMMachine
  | EggMove
  | TutorMove
  | Tradeback      -- learnable in the OTHER gen, not this one
  | EventMove
  | PreEvo         -- learnable by a pre-evolution
  | UnknownSource
  deriving (Eq, Ord, Show, Enum, Bounded)

data MoveTag = MoveTag
  { mtCategory :: !MoveCategory
  , mtLabel    :: !Text         -- "L21", "TM38", "HM04", "EGG", etc.
  } deriving (Eq, Show)


-- ── Game Data ───────────────────────────────────────────────────

-- | All static data for one generation, loaded from CSVs.
-- Immutable. Pass to pure functions as an argument.
data GameData = GameData
  { gdGen           :: !Gen
  , gdSpecies       :: !(Map Int Species)
  , gdMoves         :: !(Map Int Move)
  , gdMachines      :: !(Map Machine Int)        -- machine → move ID
  , gdMachineCompat :: !(Map Int (Set Machine))  -- dex → compatible machines
  , gdLevelUp       :: !(Map Int [(Int, Int)])    -- dex → [(level, move_id)]
  , gdEggMoves      :: !(Map Int (Set Int))
  , gdTutorMoves    :: !(Map Int (Set Int))
  , gdItems         :: !(Map Int Text)
  } deriving (Show)


-- ── Event Constraints ───────────────────────────────────────────

-- | A predicate on Pokemon, not a Pokemon itself.
-- Nothing = "any value is valid" (unknown / random / hatcher's).
data EventConstraint = EventConstraint
  { ecName     :: !Text
  , ecDex      :: !Int
  , ecGen      :: !Gen
  , ecLevel    :: !(Maybe Int)
  , ecMoves    :: ![Maybe Int]
  , ecOTName   :: !(Maybe Text)
  , ecOTID     :: !(Maybe Int)
  , ecHeldItem :: !(Maybe Int)
  , ecDVs      :: !DVConstraint
  , ecShiny    :: !ShinyStatus
  , ecGender   :: !GenderStatus
  , ecGames    :: ![Text]
  } deriving (Eq, Show)

data DVConstraint
  = AnyDVs
  | ExactDVs DVs
  | PartialDVs (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Eq, Show)

data ShinyStatus
  = ShinyAlways | ShinyPossible | ShinyNever | ShinyUnknown
  deriving (Eq, Show)

data GenderStatus
  = MaleOnly | FemaleOnly | EitherGender | GenderUnknown
  deriving (Eq, Show)

data EventMatch
  = ExactMatch   EventConstraint
  | PartialMatch EventConstraint [Text]  -- which fields diverged
  | NoMatch
  deriving (Eq, Show)
