-- | Interpreted save types and warnings.
--
-- All types produced by the interpretation layer live here.
-- No functions — just data definitions.

module Cinnabar.Save.Interpret.Types
  ( -- * Species and moves
    InterpretedSpecies (..)
  , InterpretedMove (..)

    -- * Eeveelution / rival
  , EeveelutionPath (..)
  , EeveelutionState (..)
  , RivalStarter (..)

    -- * Pokemon
  , InterpretedGenFields (..)
  , StatOrigin (..)
  , InterpretedPokemon (..)
  , InterpretedBox (..)

    -- * Daycare
  , InterpretedDaycare (..)

    -- * Progress
  , FlagState (..)
  , MapScriptState (..)
  , StatusCondition (..)
  , MovementMode (..)
  , InterpretedProgress (..)

    -- * Hall of Fame
  , InterpretedHoFEntry (..)
  , InterpretedHoFRecord (..)

    -- * Transient state
  , InterpretedTransient (..)

    -- * Options
  , TextSpeed (..)
  , BattleAnimation (..)
  , BattleStyle (..)
  , SoundSetting (..)
  , InterpretedOptions (..)

    -- * Top-level save
  , InterpretedSave (..)
  , InventoryEntry (..)
  , PlayTime (..)

    -- * Warnings
  , WarningContext (..)
  , SaveWarning (..)

    -- * Warning-carrying result
  , WithWarnings (..)

    -- * Fossil resolution
  , FossilResult (..)
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word8, Word16)

import Cinnabar.Types
import Cinnabar.Save.Raw (RawSaveFile)


-- ── Species and Moves ──────────────────────────────────────

data InterpretedSpecies
  = KnownSpecies !DexNumber !Species
  | UnknownSpecies !InternalIndex         -- Gen 1: internal index not in map
  | UnknownDexSpecies !DexNumber          -- Gen 2: dex number not in species table
  deriving (Eq, Show)

data InterpretedMove
  = KnownMove !MoveId !Move
  | UnknownMove !Word8
  | EmptyMove
  deriving (Eq, Show)


-- ── Eeveelution / Rival ────────────────────────────────────

data EeveelutionPath = JolteonPath | FlareonPath | VaporeonPath
  deriving (Eq, Show, Enum, Bounded)

data EeveelutionState
  = EeveelutionPending                    -- 0: not yet determined
  | EeveelutionKnown !EeveelutionPath     -- 1-3: determined
  | EeveelutionUnknown !Word8             -- 4+: carries raw byte
  deriving (Eq, Show)

data RivalStarter
  = RivalStarterSpecies !InterpretedSpecies
  | RivalEeveelution !EeveelutionState
  deriving (Eq, Show)


-- ── Pokemon ────────────────────────────────────────────────

data InterpretedGenFields
  = InterpGen1Fields
      { interpCatchRate :: !Word8
      }
  | InterpGen2Fields
      { interpHeldItem   :: !Word8
      , interpFriendship :: !Word8
      , interpPokerus    :: !Word8
      , interpCaughtData :: !Word16
      }
  deriving (Eq, Show)

data StatOrigin = StoredFromSave | ComputedFromBase
  deriving (Eq, Show)

data InterpretedPokemon = InterpretedPokemon
  { interpSpecies    :: !InterpretedSpecies
  , interpNickname   :: !GameText
  , interpOTName     :: !GameText
  , interpOTID       :: !TrainerId
  , interpLevel      :: !Level
  , interpMoves      :: ![InterpretedMove]
  , interpDVs        :: !DVs
  , interpStatExp    :: !StatExp
  , interpExp        :: !Experience
  , interpStatus     :: !StatusCondition
  , interpCurrentHP  :: !StatValue
  , interpMaxHP      :: !StatValue
  , interpAttack     :: !StatValue
  , interpDefense    :: !StatValue
  , interpSpeed      :: !StatValue
  , interpSpecial    :: !(Special StatValue)
  , interpStatOrigin :: !StatOrigin
  , interpGenFields  :: !InterpretedGenFields
  } deriving (Eq, Show)

data InterpretedBox = InterpretedBox
  { interpBoxNumber  :: !Int
  , interpBoxMembers :: ![InterpretedPokemon]
  } deriving (Eq, Show)


-- ── Daycare ────────────────────────────────────────────────

data InterpretedDaycare = InterpretedDaycare
  { daycareSpecies  :: !InterpretedSpecies
  , daycareNickname :: !GameText
  , daycareOTName   :: !GameText
  } deriving (Eq, Show)


-- ── Progress ───────────────────────────────────────────────

data FlagState = FlagState
  { flagName  :: !Text
  , flagIsSet :: !Bool
  } deriving (Eq, Show)

data MapScriptState = MapScriptState
  { scriptName :: !Text
  , scriptStep :: !Int
  } deriving (Eq, Show)

data StatusCondition
  = Healthy
  | Asleep !Int              -- turns remaining (1-7)
  | Poisoned
  | Burned
  | Frozen
  | Paralyzed
  | MultipleStatuses !Word8  -- invalid: multiple bits set simultaneously
  deriving (Eq, Show)

data MovementMode
  = Walking
  | Biking
  | Surfing
  | UnknownMovement !Word8
  deriving (Eq, Show)

data InterpretedProgress = InterpretedProgress
  { progPlayerStarter      :: !InterpretedSpecies
  , progRivalStarter       :: !RivalStarter
  , progBadges             :: ![FlagState]
  , progDefeatedGyms       :: ![FlagState]
  , progTownsVisited       :: ![FlagState]
  , progMovementMode       :: !MovementMode
  , progEventFlags         :: ![FlagState]
  , progToggleFlags        :: ![FlagState]
  , progMapScripts         :: ![MapScriptState]
  , progReceivedOldRod     :: !Bool
  , progReceivedGoodRod    :: !Bool
  , progReceivedSuperRod   :: !Bool
  , progReceivedLapras     :: !Bool
  , progReceivedStarter    :: !Bool
  , progHealedAtCenter     :: !Bool
  , progTrades             :: ![FlagState]
  , progTestBattle         :: !Bool
  , progPreventMusicChange :: !Bool
  , progTrainerWantsBattle :: !Bool
  , progUsedFly            :: !Bool
  , progStandingOnDoor     :: !Bool
  , progSteppingFromDoor   :: !Bool
  , progStandingOnWarp     :: !Bool
  , progJumpingLedge       :: !Bool
  , progSpinning           :: !Bool
  , progBeatenLorelei      :: !Bool
  , progActiveBoxSynced    :: !Bool
  } deriving (Eq, Show)


-- ── Hall of Fame ───────────────────────────────────────────

data InterpretedHoFEntry = InterpretedHoFEntry
  { hofSpecies  :: !InterpretedSpecies
  , hofLevel    :: !Level
  , hofNickname :: !GameText
  } deriving (Eq, Show)

data InterpretedHoFRecord = InterpretedHoFRecord
  { hofEntries :: ![InterpretedHoFEntry]
  } deriving (Eq, Show)


-- ── Transient State ────────────────────────────────────────

data InterpretedTransient = InterpretedTransient
  { transLetterDelay        :: !Int
  , transMusicId            :: !Int
  , transMusicBank          :: !Int
  , transContrastId         :: !Int
  , transEnemyTrainerClass  :: !Int
  , transBoulderSpriteIndex :: !Int
  , transDungeonWarpDest    :: !Int
  , transDungeonWarpUsed    :: !Int
  , transWarpedFromWarp     :: !Int
  , transWarpedFromMap      :: !Int
  , transCardKeyDoorY       :: !Int
  , transCardKeyDoorX       :: !Int
  , transTrashCanLock1      :: !Int
  , transTrashCanLock2      :: !Int
  , transCurrentMapScript   :: !Int
  } deriving (Eq, Show)


-- ── Options ────────────────────────────────────────────────

data TextSpeed = TextFast | TextMedium | TextSlow | TextSpeedUnknown !Int
  deriving (Eq, Show)

data BattleAnimation = AnimationsOn | AnimationsOff
  deriving (Eq, Show)

data BattleStyle = BattleShift | BattleSet
  deriving (Eq, Show)

data SoundSetting = Mono | Earphone1 | Earphone2 | Earphone3
  deriving (Eq, Show)

data InterpretedOptions = InterpretedOptions
  { optTextSpeed       :: !TextSpeed
  , optBattleAnimation :: !BattleAnimation
  , optBattleStyle     :: !BattleStyle
  , optSound           :: !(Maybe SoundSetting)  -- Nothing for R/B
  } deriving (Eq, Show)


-- ── Top-level Save ─────────────────────────────────────────

data InterpretedSave = InterpretedSave
  { interpPlayerName       :: !GameText
  , interpRivalName        :: !GameText
  , interpPlayerID         :: !TrainerId
  , interpMoney            :: !Int
  , interpCasinoCoins      :: !Int
  , interpPokedexOwned     :: !(Set DexNumber)
  , interpPokedexSeen      :: !(Set DexNumber)
  , interpBagItems         :: ![InventoryEntry]
  , interpBoxItems         :: ![InventoryEntry]
  , interpPlayTime         :: !PlayTime
  , interpPlayTimeMaxed    :: !Bool
  , interpCurrentBox       :: !Int
  , interpHoFCount         :: !Int
  , interpPikachuHappiness :: !(Maybe Int)     -- Nothing for R/B
  , interpPikachuMood      :: !(Maybe Int)     -- Nothing for R/B
  , interpSurfingHiScore   :: !(Maybe Int)     -- Nothing for R/B, decoded BCD
  , interpPrinterSettings  :: !(Maybe Word8)   -- Nothing for R/B, raw byte
  , interpDaycare          :: !(Maybe InterpretedDaycare)
  , interpPlayerY          :: !Int
  , interpPlayerX          :: !Int
  , interpCurrentMap       :: !Int
  , interpPreviousMap      :: !Int
  , interpLastBlackoutMap  :: !Int
  , interpSafariSteps      :: !Int
  , interpSafariBallCount  :: !Int
  , interpInSafari         :: !Bool
  , interpFossilItem       :: !(Maybe Text)
  , interpFossilResult     :: !(Maybe InterpretedSpecies)
  , interpTransient        :: !InterpretedTransient
  , interpOptions          :: !InterpretedOptions
  , interpParty            :: ![InterpretedPokemon]
  , interpPCBoxes          :: ![InterpretedBox]
  , interpHallOfFame       :: ![InterpretedHoFRecord]
  , interpActiveBoxNum     :: !Int
  , interpProgress         :: !InterpretedProgress
  , interpWarnings         :: ![SaveWarning]
  , interpRaw              :: !RawSaveFile
  }


-- ── Sub-records ────────────────────────────────────────────

data InventoryEntry = InventoryEntry
  { entryName     :: !Text
  , entryQuantity :: !Int
  } deriving (Eq, Show)

data PlayTime = PlayTime
  { playHours   :: !Int
  , playMinutes :: !Int
  , playSeconds :: !Int
  } deriving (Eq, Show)


-- ── Warnings ───────────────────────────────────────────────

data WarningContext
  = PartySlot !Int           -- 1-based slot index
  | BoxSlot !Int !Int        -- 1-based box number, 1-based slot index
  | HoFSlot !Int !Int        -- 1-based record index, 1-based entry index
  | PlayerStarter
  | RivalStarterSlot
  | DaycareSlot
  | FossilSlot
  deriving (Eq, Show)

data SaveWarning
  = UnknownSpeciesIndex !WarningContext !InternalIndex
  | UnknownMoveId !WarningContext !Int !Word8         -- context, move slot (1-based), byte
  | SpeciesListMismatch !WarningContext !Word8 !Word8
  | ChecksumMismatch !Word8 !Word8
  | StatMismatch !WarningContext !Text !StatValue !StatValue  -- context, stat name, stored, calculated
  | BoxBankChecksumMismatch !Int !Word8 !Word8        -- bank index, stored, calculated
  | BoxChecksumMismatch !Int !Int !Word8 !Word8       -- bank index, box-within-bank index, stored, calculated
  | ActiveBoxDesync
  | UnexpectedEeveelution !Word8
  | CountExceedsCapacity !Text !Int !Int   -- field name, raw count, capacity
  deriving (Eq, Show)


-- ── Warning-Carrying Result ────────────────────────────────

-- | A computed result paired with any warnings encountered
-- during interpretation.
data WithWarnings a = WithWarnings
  { computedResult      :: a
  , encounteredWarnings :: [SaveWarning]
  } deriving (Show)


-- ── Fossil Resolution ──────────────────────────────────────

data FossilResult = FossilResult
  { fossilItemName :: !(Maybe Text)
  , fossilSpecies  :: !(Maybe InterpretedSpecies)
  } deriving (Show)
