{-# LANGUAGE OverloadedStrings #-}

-- | Core domain types for cinnabar-coast.
--
-- Pure data definitions: no IO, no parsing, no formatting.
-- Every value is immutable. Types model the domain, not the
-- binary save format — the parser layer handles translation.

module Pokemon.Types
  ( -- * Domain IDs
    DexNumber (..)
  , MoveId (..)
  , Level (..)
  , ItemId (..)
  , TrainerId (..)

    -- * Generation
  , Gen (..)

    -- * Pokemon Types
  , PokemonType (..)

    -- * Growth Rate
  , GrowthRate (..)

    -- * Gender Ratio
  , GenderRatio (..)
  , genderThreshold

    -- * Egg Group
  , EggGroup (..)

    -- * Type pair
  , TypePair (..)

    -- * Egg group pair
  , EggGroupPair (..)

    -- * Level-up entry
  , LevelUpEntry (..)

    -- * Species (static game data)
  , Species (..)
  , BaseStats (..)
  , Special (..)
  , specialAttack, specialDefense

    -- * Moves (static game data)
  , MoveType (..)
  , Move (..)
  , MoveSlot (..)
  , mkMoveSlots

    -- * DVs and Stat Exp
  , DVs (..)
  , dvHP, maxDVs, isShiny
  , StatExp (..)
  , zeroStatExp, maxStatExp

    -- * Pokemon (save file instance)
  , Pokemon (..)
  , GenData (..)
  , Gen1Fields (..)
  , Gen2Fields (..)

    -- * Evolution
  , EvoTrigger (..)
  , EvolutionStep (..)

    -- * TM / HM
  , Machine (..)

    -- * Move classification
  , LearnMethod (..)
  , LearnSource (..)

    -- * Game data (loaded per generation)
  , GameData (..)

    -- * Text codec
  , Language (..)
  , GameChar (..)
  , GameText (..)

    -- * Event constraints
  , EventConstraint (..)
  , DVConstraint (..)
  , ShinyStatus (..)
  , GenderStatus (..)
  , EventMatch (..)
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word8, Word16)


-- ── Domain IDs ─────────────────────────────────────────────────

newtype DexNumber  = DexNumber  { unDex       :: Int } deriving (Eq, Ord, Show)
newtype MoveId     = MoveId     { unMoveId    :: Int } deriving (Eq, Ord, Show)
newtype Level      = Level      { unLevel     :: Int } deriving (Eq, Ord, Show)
newtype ItemId     = ItemId     { unItemId    :: Int } deriving (Eq, Ord, Show)
newtype TrainerId  = TrainerId  { unTrainerId :: Int } deriving (Eq, Ord, Show)


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
  | MediumSlow    -- ⁶⁄₅n³ − 15n² + 100n − 140
  | Fast          -- ⅘n³
  | Slow          -- ⁵⁄₄n³
  deriving (Eq, Ord, Show)


-- ── Gender Ratio ──────────────────────────────────────────────────

-- | Gender ratio as stored in the ROM. The byte value is the threshold
-- for DV comparison: if Attack DV * 17 > threshold, the Pokémon is male.
data GenderRatio
  = AllMale         -- 0% female
  | Female12_5      -- 12.5% female
  | Female25        -- 25% female
  | Female50        -- 50% female
  | Female75        -- 75% female
  | AllFemale       -- 100% female
  | Genderless      -- no gender (Magnemite, legendaries)
  deriving (Eq, Ord, Show)

-- | The raw byte threshold for gender determination.
-- Used by stat calculation: Attack DV * 17 > threshold → male.
genderThreshold :: GenderRatio -> Int
genderThreshold AllMale    = 0
genderThreshold Female12_5 = 31
genderThreshold Female25   = 63
genderThreshold Female50   = 127
genderThreshold Female75   = 191
genderThreshold AllFemale  = 254
genderThreshold Genderless = 255


-- ── Egg Group ─────────────────────────────────────────────────────

-- | Egg compatibility group. Every Gen 2 species has exactly two
-- egg group slots — legendaries use EggNone. The outer Maybe on
-- Species distinguishes "Gen 1, no egg groups" from "Gen 2, has slots."
data EggGroup
  = EggMonster | EggWater1 | EggBug | EggFlying | EggGround
  | EggFairy | EggPlant | EggHumanShape | EggWater3
  | EggMineral | EggIndeterminate | EggWater2
  | EggDitto | EggDragon | EggNone
  deriving (Eq, Ord, Show)


-- ── Type Pair ───────────────────────────────────────────────────

data TypePair = TypePair
  { primaryType   :: !PokemonType
  , secondaryType :: !PokemonType
  } deriving (Eq, Show)


-- ── Egg Group Pair ──────────────────────────────────────────────

data EggGroupPair = EggGroupPair
  { primaryEggGroup   :: !EggGroup
  , secondaryEggGroup :: !EggGroup
  } deriving (Eq, Show)


-- ── Level-Up Entry ──────────────────────────────────────────────

data LevelUpEntry = LevelUpEntry
  { learnLevel :: !Level
  , learnMove  :: !MoveId
  } deriving (Eq, Show)


-- ── Base Stats ──────────────────────────────────────────────────

data BaseStats = BaseStats
  { baseHP      :: !Int
  , baseAttack  :: !Int
  , baseDefense :: !Int
  , baseSpeed   :: !Int
  , baseSpecial :: !Special
  } deriving (Eq, Show)

-- | Gen 1 has one Special stat; Gen 2 splits it into SpAtk / SpDef.
data Special
  = Unified !Int          -- Gen 1
  | Split   !Int !Int     -- Gen 2: (SpAtk, SpDef)
  deriving (Eq, Show)

specialAttack :: Special -> Int
specialAttack (Unified value)  = value
specialAttack (Split spAttack _) = spAttack

specialDefense :: Special -> Int
specialDefense (Unified value)  = value
specialDefense (Split _ spDefense) = spDefense


-- ── Species ─────────────────────────────────────────────────────

data Species = Species
  { speciesDex           :: !DexNumber
  , speciesName          :: !Text
  , speciesBaseStats     :: !BaseStats
  , speciesTypes         :: !TypePair
  , speciesCatchRate     :: !Int
  , speciesGrowthRate    :: !GrowthRate
  -- Gen 2 fields. Nothing for Gen 1 entries.
  , speciesGenderRatio   :: !(Maybe GenderRatio)
  , speciesEggGroups     :: !(Maybe EggGroupPair)
  , speciesBaseHappiness :: !(Maybe Int)
  } deriving (Eq, Show)


-- ── Move ────────────────────────────────────────────────────────

-- | The type of a move. Most moves have a standard PokemonType, but
-- Curse uses CURSE_TYPE (0x13 in the ROM, displays as "???") which
-- doesn't correspond to any real element type. UnknownType preserves
-- unrecognized type bytes for round-tripping.
data MoveType
  = StandardType !PokemonType
  | CurseType                    -- ^ the ??? type, only used by Curse
  | UnknownType !Word8           -- ^ unrecognized byte, preserved for round-tripping
  deriving (Eq, Show)

data Move = Move
  { moveId       :: !MoveId
  , moveName     :: !Text
  , moveType     :: !MoveType
  , movePower    :: !Int    -- 0 for status moves
  , moveAccuracy :: !Int    -- 0 for never-miss
  , movePP       :: !Int    -- base PP before PP Ups
  } deriving (Eq, Show)


-- ── DVs ─────────────────────────────────────────────────────────

-- | Determinant Values. Same 4 values in both gens.
-- dvSpecial governs both SpAtk and SpDef in Gen 2 — the split only
-- affects base stats and stat calculation, not DV storage.
data DVs = DVs
  { dvAttack  :: !Int    -- 0–15
  , dvDefense :: !Int
  , dvSpeed   :: !Int
  , dvSpecial :: !Int    -- SpAtk AND SpDef in Gen 2
  } deriving (Eq, Show)

-- | HP DV: derived from the low bits of the other four.
dvHP :: DVs -> Int
dvHP dvs =
      (dvAttack dvs  .&. 1) `shiftL` 3
  .|. (dvDefense dvs .&. 1) `shiftL` 2
  .|. (dvSpeed dvs   .&. 1) `shiftL` 1
  .|. (dvSpecial dvs .&. 1)

maxDVs :: DVs
maxDVs = DVs 15 15 15 15

-- | Shiny (Gen 2): Def=10, Spd=10, Spc=10, Atk bit 1 set.
isShiny :: DVs -> Bool
isShiny dvs =
  dvDefense dvs == 10 && dvSpeed dvs == 10 && dvSpecial dvs == 10
  && (dvAttack dvs .&. 2) /= 0


-- ── Stat Exp ────────────────────────────────────────────────────

-- | Stat Experience. Same 5 values in both gens.
-- expSpecial applies to both SpAtk and SpDef calcs in Gen 2.
data StatExp = StatExp
  { expHP      :: !Int    -- 0–65535
  , expAttack  :: !Int
  , expDefense :: !Int
  , expSpeed   :: !Int
  , expSpecial :: !Int
  } deriving (Eq, Show)

zeroStatExp :: StatExp
zeroStatExp = StatExp 0 0 0 0 0

maxStatExp :: StatExp
maxStatExp = StatExp 65535 65535 65535 65535 65535


-- ── Move Slot ───────────────────────────────────────────────────

-- | One of four move slots. Nothing = empty slot.
-- slotCurrentPP is remaining uses (numerator). Max PP is derived:
--   max_pp = base_pp + (base_pp * slotPPUps / 5)
data MoveSlot = MoveSlot
  { slotMoveId   :: !MoveId -- move ID
  , slotPPUps    :: !Int    -- 0–3
  , slotCurrentPP :: !Int   -- 0–63, remaining uses
  } deriving (Eq, Show)

-- | Smart constructor: validates 1–4 moves.
mkMoveSlots :: [MoveSlot] -> Either Text (NonEmpty MoveSlot)
mkMoveSlots [] = Left "Pokemon must have at least one move"
mkMoveSlots moves
  | length moves > 4 = Left "Pokemon cannot have more than four moves"
  | otherwise = Right (NE.fromList moves)


-- ── Pokemon ─────────────────────────────────────────────────────

data Pokemon = Pokemon
  { pokemonDex      :: !DexNumber
  , pokemonNickname :: !Text
  , pokemonOTName   :: !Text
  , pokemonOTID     :: !TrainerId -- 0–65535
  , pokemonLevel    :: !Level     -- 1–100
  , pokemonExp      :: !Int       -- 0–16,777,215
  , pokemonMoves    :: !(NonEmpty MoveSlot)
  , pokemonDVs      :: !DVs
  , pokemonStatExp  :: !StatExp
  , pokemonStatus   :: !Word8
  , pokemonGenData  :: !GenData
  } deriving (Eq, Show)

-- | Gen-specific fields. Pattern match to handle each gen.
data GenData
  = Gen1Data !Gen1Fields
  | Gen2Data !Gen2Fields
  deriving (Eq, Show)

data Gen1Fields = Gen1Fields
  { gen1SpeciesIndex :: !Word8
  , gen1Type1        :: !PokemonType
  , gen1Type2        :: !PokemonType
  , gen1CatchRate    :: !Word8
  } deriving (Eq, Show)

data Gen2Fields = Gen2Fields
  { gen2HeldItem   :: !Word8
  , gen2Friendship :: !Word8
  , gen2Pokerus    :: !Word8
  , gen2CaughtData :: !Word16
  } deriving (Eq, Show)


-- ── Evolution ─────────────────────────────────────────────────

-- | What triggers an evolution.
data EvoTrigger
  = EvoLevel !Level         -- reach this level
  | EvoItem !Text           -- use item (constant name, e.g. "MOON_STONE")
  | EvoTrade                -- trade (no held item required)
  | EvoTradeItem !Text      -- trade holding item (Gen 2, e.g. "KINGS_ROCK")
  | EvoHappiness            -- friendship ≥ 220, any time
  | EvoHappinessDay         -- friendship + daytime (Gen 2)
  | EvoHappinessNight       -- friendship + nighttime (Gen 2)
  | EvoStatLT !Level        -- level + Atk < Def (Tyrogue → Hitmonchan)
  | EvoStatGT !Level        -- level + Atk > Def (Tyrogue → Hitmonlee)
  | EvoStatEQ !Level        -- level + Atk = Def (Tyrogue → Hitmontop)
  deriving (Eq, Show)

-- | One evolution step: species A evolves into species B when
-- a trigger condition is met.
data EvolutionStep = EvolutionStep
  { stepFrom    :: !DexNumber    -- dex number of the source
  , stepTo      :: !DexNumber    -- dex number of the result
  , stepTrigger :: !EvoTrigger
  } deriving (Eq, Show)


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
data LearnMethod
  = LevelUp
  | TMMachine
  | HMMachine
  | EggMove
  | TutorMove
  | Tradeback      -- learnable in the OTHER gen, not this one
  | EventMove
  | PreEvo         -- learnable by a pre-evolution
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One specific way a species can learn a move.
-- sourceVia is empty for direct sources (level-up, TM, egg, tutor).
-- For Tradeback and PreEvo it carries the nested sources that
-- explain WHY — e.g. Tradeback "Gen 1" [LearnSource TMMachine "TM24" []]
-- means "learnable by trading to Gen 1, where TM24 teaches it."
data LearnSource = LearnSource
  { sourceMethod :: !LearnMethod
  , sourceDetail :: !Text
  , sourceVia    :: [LearnSource]
  } deriving (Eq, Show)


-- ── Game Data ───────────────────────────────────────────────────

-- | All static data for one generation, loaded from CSVs.
-- Immutable. Pass to pure functions as an argument.
data GameData = GameData
  { gameGen           :: !Gen
  , gameSpecies       :: !(Map DexNumber Species)
  , gameSpeciesByName :: !(Map Text DexNumber)          -- name → dex number
  , gameMoves         :: !(Map MoveId Move)
  , gameMoveByName    :: !(Map Text MoveId)             -- name → move ID
  , gameMachines      :: !(Map Machine MoveId)          -- machine → move ID
  , gameMachineCompat :: !(Map DexNumber (Set Machine)) -- dex → compatible machines
  , gameLevelUp       :: !(Map DexNumber [LevelUpEntry])  -- dex → level-up learnset
  , gameEggMoves      :: !(Map DexNumber (Set MoveId))
  , gameTutorMoves    :: !(Map DexNumber (Set MoveId))
  , gameItems         :: !(Map ItemId Text)
  , gameEvolvesInto   :: !(Map DexNumber [EvolutionStep]) -- dex → what it evolves into
  , gameEvolvesFrom   :: !(Map DexNumber [EvolutionStep]) -- dex → what evolves into it
  } deriving (Show)


-- ── Language ──────────────────────────────────────────────────────

-- | Game cartridge language. Determines which character encoding
-- to use. EN/FR/IT/ES share some encodings in some gens, but
-- the caller doesn't need to know — the codec loader resolves it.
data Language = English | French | German | Italian | Spanish | Japanese
  deriving (Eq, Ord, Show, Enum, Bounded)


-- ── Game Text ─────────────────────────────────────────────────────

-- | One character decoded from Game Boy text. Preserves the
-- distinction between regular characters, multi-char ligatures
-- (PK, MN, contractions), and unrecognized bytes.
data GameChar
  = Literal !Char         -- ^ Single Unicode character
  | Ligature !Text        -- ^ Multi-char glyph: "PK", "MN", "'d", etc.
  | UnknownByte !Word8    -- ^ Unrecognized byte, preserved for round-tripping
  deriving (Eq, Ord, Show)

-- | A decoded Game Boy text string. Lossless representation —
-- use displayText to convert to human-readable Text.
newtype GameText = GameText { gameTextChars :: [GameChar] }
  deriving (Eq, Show)


-- ── Event Constraints ───────────────────────────────────────────

-- | A predicate on Pokemon, not a Pokemon itself.
-- Nothing = "any value is valid" (unknown / random / hatcher's).
data EventConstraint = EventConstraint
  { eventName     :: !Text
  , eventDex      :: !DexNumber
  , eventGen      :: !Gen
  , eventLevel    :: !(Maybe Level)
  , eventMoves    :: ![Maybe MoveId]
  , eventOTName   :: !(Maybe Text)
  , eventOTID     :: !(Maybe TrainerId)
  , eventHeldItem :: !(Maybe ItemId)
  , eventDVs      :: !DVConstraint
  , eventShiny    :: !ShinyStatus
  , eventGender   :: !GenderStatus
  , eventGames    :: ![Text]
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
