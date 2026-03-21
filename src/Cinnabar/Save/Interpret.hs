{-# LANGUAGE OverloadedStrings #-}

-- | Interpretation layer: raw parsed save → domain values.
--
-- The second pass of the two-pass architecture. Takes a raw parse
-- (bytes, offsets, packed fields) and resolves it against GameData
-- and TextCodec to produce human-meaningful types. Interpretation
-- is partial — one unresolvable field doesn't prevent interpreting
-- the rest. Problems accumulate as warnings, never errors.
--
-- Types are gen-agnostic. Only the interpretation function is
-- gen-specific: interpretGen1Save now, interpretGen2Save later.

module Cinnabar.Save.Interpret
  ( -- * Interpreted types
    InterpretedSpecies (..)
  , InterpretedMove (..)
  , EeveelutionPath (..)
  , EeveelutionState (..)
  , RivalStarter (..)
  , InterpretedGenFields (..)
  , StatOrigin (..)
  , InterpretedPokemon (..)
  , InterpretedBox (..)
  , FlagState (..)
  , MapScriptState (..)
  , StatusCondition (..)
  , MovementMode (..)
  , InterpretedProgress (..)
  , InterpretedHoFEntry (..)
  , InterpretedDaycare (..)
  , InterpretedTransient (..)
  , InterpretedHoFRecord (..)
  , TextSpeed (..)
  , BattleAnimation (..)
  , BattleStyle (..)
  , SoundSetting (..)
  , InterpretedOptions (..)
  , InterpretedSave (..)

    -- * Decoded sub-records
  , InventoryEntry (..)
  , PlayTime (..)

    -- * Warnings
  , WarningContext (..)
  , SaveWarning (..)

    -- * Interpretation
  , interpretStatus
  , interpretGen1Save
  ) where

import Data.Bits (testBit, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.List (zip4)
import Data.Maybe (catMaybes)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16)

import Cinnabar.Types
import Cinnabar.Stats (CalcStats (..), calcAllStats)
import Cinnabar.TextCodec (TextCodec, decodeText, showHexByte)
import Cinnabar.Save.Layout (CartridgeLayout (..), GameVariant (..))
import Cinnabar.Save.Raw
  ( RawSaveFile (..), RawGen1SaveFile (..), RawGen1Party (..)
  , RawGen1Box (..), RawBankValidity (..)
  , RawItemEntry (..), RawPlayTime (..), RawDaycare (..)
  , RawGen1HoFEntry (..), RawGen1HoFRecord (..)
  , RawProgressFlags (..)
  , RawPlayerPosition (..), RawSafariState (..), RawFossilState (..)
  , RawTransientState (..)
  )
import Cinnabar.Save.Gen1.Raw (RawGen1PartyPokemon (..), RawGen1BoxPokemon (..), RawStatExp (..))


-- ── Interpreted Types ────────────────────────────────────────

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
  , interpExp        :: !Int
  , interpStatus     :: !StatusCondition
  , interpCurrentHP  :: !Int
  , interpMaxHP      :: !Int
  , interpAttack     :: !Int
  , interpDefense    :: !Int
  , interpSpeed      :: !Int
  , interpSpecial    :: !Special
  , interpStatOrigin :: !StatOrigin
  , interpGenFields  :: !InterpretedGenFields
  } deriving (Eq, Show)

data InterpretedBox = InterpretedBox
  { interpBoxNumber :: !Int
  , interpBoxMembers   :: ![InterpretedPokemon]
  } deriving (Eq, Show)

data InterpretedDaycare = InterpretedDaycare
  { daycareSpecies  :: !InterpretedSpecies
  , daycareNickname :: !GameText
  , daycareOTName   :: !GameText
  } deriving (Eq, Show)

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

-- | Decode the status byte. Bits 0-2 are sleep turns, bits 3-6 are
-- PSN/BRN/FRZ/PAR (mutually exclusive). The game enforces at most one
-- condition, but a save editor could create invalid combinations.
interpretStatus :: Word8 -> StatusCondition
interpretStatus byte
  | sleepTurns /= 0 && statusBits /= 0 = MultipleStatuses byte
  | sleepTurns /= 0                     = Asleep sleepTurns
  | statusBits == 0                     = Healthy
  | statusBits == 0x08                  = Poisoned
  | statusBits == 0x10                  = Burned
  | statusBits == 0x20                  = Frozen
  | statusBits == 0x40                  = Paralyzed
  | otherwise                           = MultipleStatuses byte
  where
    sleepTurns = fromIntegral (byte .&. 0x07)
    statusBits = byte .&. 0xF8  -- bits 3-7

data MovementMode
  = Walking
  | Biking
  | Surfing
  | UnknownMovement !Word8
  deriving (Eq, Show)

data InterpretedProgress = InterpretedProgress
  { progPlayerStarter     :: !InterpretedSpecies
  , progRivalStarter      :: !RivalStarter
  , progBadges            :: ![FlagState]
  , progDefeatedGyms      :: ![FlagState]
  , progTownsVisited      :: ![FlagState]
  , progMovementMode      :: !MovementMode
  , progEventFlags        :: ![FlagState]
  , progToggleFlags       :: ![FlagState]
  , progMapScripts        :: ![MapScriptState]
  , progReceivedOldRod    :: !Bool
  , progReceivedGoodRod   :: !Bool
  , progReceivedSuperRod  :: !Bool
  , progReceivedLapras    :: !Bool
  , progReceivedStarter   :: !Bool
  , progHealedAtCenter    :: !Bool
  , progTrades            :: ![FlagState]
  , progTestBattle          :: !Bool
  , progPreventMusicChange  :: !Bool
  , progTrainerWantsBattle  :: !Bool
  , progUsedFly             :: !Bool
  , progStandingOnDoor      :: !Bool
  , progSteppingFromDoor    :: !Bool
  , progStandingOnWarp      :: !Bool
  , progJumpingLedge        :: !Bool
  , progSpinning            :: !Bool
  , progBeatenLorelei       :: !Bool
  , progActiveBoxSynced     :: !Bool
  } deriving (Eq, Show)

data InterpretedHoFEntry = InterpretedHoFEntry
  { hofSpecies  :: !InterpretedSpecies
  , hofLevel    :: !Level
  , hofNickname :: !GameText
  } deriving (Eq, Show)

data InterpretedHoFRecord = InterpretedHoFRecord
  { hofEntries :: ![InterpretedHoFEntry]
  } deriving (Eq, Show)

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

data InterpretedSave = InterpretedSave
  { interpPlayerName    :: !GameText
  , interpRivalName     :: !GameText
  , interpPlayerID      :: !TrainerId
  , interpMoney         :: !Int
  , interpCasinoCoins   :: !Int
  , interpPokedexOwned  :: !(Set DexNumber)
  , interpPokedexSeen   :: !(Set DexNumber)
  , interpBagItems      :: ![InventoryEntry]
  , interpBoxItems      :: ![InventoryEntry]
  , interpPlayTime      :: !PlayTime
  , interpPlayTimeMaxed :: !Bool
  , interpCurrentBox    :: !Int
  , interpHoFCount      :: !Int
  , interpPikachuHappiness :: !(Maybe Int)     -- Nothing for R/B
  , interpPikachuMood      :: !(Maybe Int)     -- Nothing for R/B
  , interpSurfingHiScore   :: !(Maybe Int)     -- Nothing for R/B, decoded BCD
  , interpPrinterSettings  :: !(Maybe Word8)   -- Nothing for R/B, raw byte
  , interpDaycare         :: !(Maybe InterpretedDaycare)
  , interpPlayerY         :: !Int
  , interpPlayerX         :: !Int
  , interpCurrentMap      :: !Int
  , interpPreviousMap     :: !Int
  , interpLastBlackoutMap :: !Int
  , interpSafariSteps     :: !Int
  , interpSafariBallCount :: !Int
  , interpInSafari        :: !Bool
  , interpFossilItem      :: !(Maybe Text)
  , interpFossilResult    :: !(Maybe InterpretedSpecies)
  , interpTransient       :: !InterpretedTransient
  , interpOptions         :: !InterpretedOptions
  , interpParty         :: ![InterpretedPokemon]
  , interpPCBoxes       :: ![InterpretedBox]
  , interpHallOfFame    :: ![InterpretedHoFRecord]
  , interpActiveBoxNum  :: !Int
  , interpProgress      :: !InterpretedProgress
  , interpWarnings      :: ![SaveWarning]
  , interpRaw           :: !RawSaveFile
  }

-- | Resolved inventory item.
data InventoryEntry = InventoryEntry
  { entryName     :: !Text
  , entryQuantity :: !Int
  } deriving (Eq, Show)

data PlayTime = PlayTime
  { playHours   :: !Int
  , playMinutes :: !Int
  , playSeconds :: !Int
  } deriving (Eq, Show)


-- ── Warnings ─────────────────────────────────────────────────

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
  | StatMismatch !WarningContext !Text !Int !Int      -- context, stat name, stored, calculated
  | BoxBankChecksumMismatch !Int !Word8 !Word8        -- bank index, stored, calculated
  | BoxChecksumMismatch !Int !Int !Word8 !Word8       -- bank index, box-within-bank index, stored, calculated
  | ActiveBoxDesync
  | UnexpectedEeveelution !Word8
  deriving (Eq, Show)


-- ── Gen 1 Interpretation ─────────────────────────────────────

interpretGen1Save :: GameData -> TextCodec -> RawGen1SaveFile -> InterpretedSave
interpretGen1Save gameData codec rawSave =
  let speciesGraph  = gameSpeciesGraph gameData
      lookupTables  = gameLookupTables gameData
      indexMap       = gameInternalIndex speciesGraph
      speciesMap     = gameSpecies speciesGraph
      moveMap        = gameMoves lookupTables
      itemMap        = gameItems lookupTables
      machineMap     = gameMachines (gameMachineData gameData)
      party          = rawGen1Party rawSave
      partyCount     = fromIntegral (rawGen1PartyCount party)
      gameVariant    = layoutGame (rawGen1Layout rawSave)

      namePairs = zipWith RawNamePair
        (rawGen1PartyOTNames party)
        (rawGen1PartyNicknames party)

      indexedSlots = zip4
        [0 ..]
        (rawGen1PartySpecies party)
        (rawGen1PartyMembers party)
        namePairs

      (interpretedMembers, pokemonWarnings) = unzip
        [ interpretGen1Pokemon indexMap speciesMap moveMap codec
            idx listSpec pokemon names
        | (idx, listSpec, pokemon, names) <- indexedSlots
        ]

      checksumWarnings
        | rawGen1Checksum rawSave == rawGen1CalculatedChecksum rawSave = []
        | otherwise = [ChecksumMismatch (rawGen1Checksum rawSave)
                                        (rawGen1CalculatedChecksum rawSave)]

      currentBoxNumber = fromIntegral (rawGen1CurrentBoxNum rawSave .&. 0x7F) + 1

      rawPlayTimeRecord = rawGen1PlayTime rawSave
      daycareRecord     = rawGen1Daycare rawSave
      (interpretedDaycare, daycareWarnings) = resolveDaycare indexMap speciesMap codec daycareRecord

      -- Player position
      positionRecord = rawGen1PlayerPosition rawSave

      -- Safari state
      safariRecord = rawGen1Safari rawSave

      -- Fossil resolution
      fossilRecord = rawGen1Fossil rawSave
      (fossilItemName, fossilSpecies, fossilWarnings) =
        resolveFossil indexMap speciesMap itemMap fossilRecord

      -- Transient state
      transientRecord = rawGen1Transient rawSave

      -- PC box interpretation (non-empty boxes only)
      (interpretedBoxes, boxPokemonWarnings) = unzip
        [ ( InterpretedBox { interpBoxNumber = boxNum, interpBoxMembers = boxMembers }
          , concat perPokemonWarnings
          )
        | (boxIdx, rawBox) <- zip [0 :: Int ..] (rawGen1PCBoxes rawSave)
        , let boxNum   = boxIdx + 1
              boxCount = fromIntegral (rawGen1BoxCount rawBox) :: Int
        , boxCount > 0
        , let boxNamePairs = zipWith RawNamePair
                (rawGen1BoxOTNames rawBox)
                (rawGen1BoxNicknames rawBox)
              boxSlots = zip4
                [0 ..]
                (rawGen1BoxSpecies rawBox)
                (rawGen1BoxMembers rawBox)
                boxNamePairs
              (boxMembers, perPokemonWarnings) = unzip
                [ interpretGen1BoxPokemon indexMap speciesMap moveMap codec
                    boxNum idx listSpec pokemon names
                | (idx, listSpec, pokemon, names) <- boxSlots
                ]
        ]

      -- Hall of Fame interpretation
      hofCount = fromIntegral (rawGen1HoFCount rawSave) :: Int
      (interpretedHoF, hofWarnings) = unzip
        [ interpretHoFRecord indexMap speciesMap codec recordIndex record
        | (recordIndex, record) <- zip [1 ..] (take hofCount (rawGen1HallOfFame rawSave))
        ]

      -- Box bank checksum warnings
      boxBankWarnings = concat
        [ bankWarning ++ perBoxWarnings
        | (bankIdx, validity) <- zip [0 :: Int ..] (rawGen1BoxBankValid rawSave)
        , let bankWarning
                | bankStoredChecksum validity == bankCalculatedChecksum validity = []
                | otherwise = [BoxBankChecksumMismatch bankIdx
                    (bankStoredChecksum validity) (bankCalculatedChecksum validity)]
              perBoxWarnings =
                [ BoxChecksumMismatch bankIdx boxInBankIdx stored calculated
                | (boxInBankIdx, (stored, calculated)) <-
                    zip [0 :: Int ..] (boxChecksumPairs validity)
                , stored /= calculated
                ]
        ]

      -- Progress interpretation
      (progress, progressSpeciesWarnings) = interpretProgress gameData rawSave

      progressWarnings
        | progActiveBoxSynced progress = []
        | otherwise = [ActiveBoxDesync]

  in InterpretedSave
      { interpPlayerName    = decodeText codec (rawGen1PlayerName rawSave)
      , interpRivalName     = decodeText codec (rawGen1RivalName rawSave)
      , interpPlayerID      = TrainerId (fromIntegral (rawGen1PlayerID rawSave))
      , interpMoney         = decodeBCD (rawGen1Money rawSave)
      , interpCasinoCoins   = decodeBCD (rawGen1CasinoCoins rawSave)
      , interpPokedexOwned  = decodePokedexFlags (rawGen1PokedexOwned rawSave)
      , interpPokedexSeen   = decodePokedexFlags (rawGen1PokedexSeen rawSave)
      , interpBagItems      = resolveItems itemMap machineMap moveMap (rawGen1BagItems rawSave)
      , interpBoxItems      = resolveItems itemMap machineMap moveMap (rawGen1BoxItems rawSave)
      , interpPlayTime      = promotePlayTime rawPlayTimeRecord
      , interpPlayTimeMaxed = rawPlayMaxed rawPlayTimeRecord /= 0
      , interpCurrentBox    = currentBoxNumber
      , interpHoFCount      = fromIntegral (rawGen1HoFCount rawSave)
      , interpPikachuHappiness = yellowOnly gameVariant (fromIntegral (rawGen1PikachuHappiness rawSave))
      , interpPikachuMood     = yellowOnly gameVariant (fromIntegral (rawGen1PikachuMood rawSave))
      , interpSurfingHiScore  = yellowOnly gameVariant (decodeBCDLE (rawGen1SurfingHiScore rawSave))
      , interpPrinterSettings = yellowOnly gameVariant (rawGen1PrinterSettings rawSave)
      , interpDaycare         = interpretedDaycare
      , interpPlayerY         = fromIntegral (rawPlayerY positionRecord)
      , interpPlayerX         = fromIntegral (rawPlayerX positionRecord)
      , interpCurrentMap      = fromIntegral (rawCurrentMap (rawGen1Progress rawSave))
      , interpPreviousMap     = fromIntegral (rawLastMap positionRecord)
      , interpLastBlackoutMap = fromIntegral (rawLastBlackoutMap positionRecord)
      , interpSafariSteps     = fromIntegral (rawSafariSteps safariRecord)
      , interpSafariBallCount = fromIntegral (rawSafariBallCount safariRecord)
      , interpInSafari        = rawSafariGameOver safariRecord == 0
                             && rawSafariSteps safariRecord > 0
      , interpFossilItem      = fossilItemName
      , interpFossilResult    = fossilSpecies
      , interpTransient       = promoteTransient transientRecord
      , interpOptions         = interpretOptions gameVariant (rawGen1Options rawSave)
      , interpParty         = take partyCount interpretedMembers
      , interpPCBoxes       = interpretedBoxes
      , interpHallOfFame    = interpretedHoF
      , interpActiveBoxNum  = currentBoxNumber
      , interpProgress      = progress
      , interpWarnings      = concat pokemonWarnings ++ checksumWarnings
                           ++ concat boxPokemonWarnings ++ boxBankWarnings
                           ++ concat hofWarnings ++ daycareWarnings
                           ++ fossilWarnings
                           ++ progressSpeciesWarnings ++ progressWarnings
      , interpRaw           = RawGen1Save rawSave
      }


-- ── Per-Pokemon Interpretation ───────────────────────────────────

data RawNamePair = RawNamePair
  { rawOTNameBytes :: !ByteString
  , rawNickBytes   :: !ByteString
  } deriving (Eq, Show)

interpretGen1Pokemon
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Map.Map MoveId Move
  -> TextCodec
  -> Int                              -- slot index
  -> InternalIndex                    -- species list byte
  -> RawGen1PartyPokemon                  -- struct data
  -> RawNamePair                      -- OT name and nickname bytes
  -> (InterpretedPokemon, [SaveWarning])
interpretGen1Pokemon indexMap speciesMap moveMap codec
                 slotIndex listSpecies partyPokemon namePair =
  let otNameBytes = rawOTNameBytes namePair
      nickBytes   = rawNickBytes namePair
      structSpecies = rawG1SpeciesIndex partyPokemon
      dvs           = unpackDVs (rawG1DVBytes partyPokemon)
      level         = Level (fromIntegral (rawG1Level partyPokemon))
      promotedExp   = promoteStatExp (rawG1StatExp partyPokemon)

      -- Species resolution
      context = PartySlot (slotIndex + 1)
      (interpSpecies, speciesWarnings) = resolveSpecies indexMap speciesMap context structSpecies

      -- Species list cross-check (compare raw bytes)
      listByte   = unInternalIndex listSpecies
      structByte = unInternalIndex structSpecies
      listWarnings
        | listByte == structByte = []
        | otherwise = [SpeciesListMismatch context listByte structByte]

      -- Move resolution
      rawMoveBytes = [rawG1Move1 partyPokemon, rawG1Move2 partyPokemon,
                      rawG1Move3 partyPokemon, rawG1Move4 partyPokemon]
      (interpMoves, moveWarnings) = resolveMoves moveMap context rawMoveBytes

      -- Stat cross-check
      storedStats = StoredStats
        { storedMaxHP   = fromIntegral (rawG1MaxHP partyPokemon)
        , storedAttack  = fromIntegral (rawG1Attack partyPokemon)
        , storedDefense = fromIntegral (rawG1Defense partyPokemon)
        , storedSpeed   = fromIntegral (rawG1Speed partyPokemon)
        , storedSpecial = Unified (fromIntegral (rawG1Special partyPokemon))
        }
      statWarnings = case interpSpecies of
        KnownSpecies _ species -> checkStats context species dvs promotedExp level storedStats
        _                      -> []

  in ( InterpretedPokemon
        { interpSpecies    = interpSpecies
        , interpNickname   = decodeText codec nickBytes
        , interpOTName     = decodeText codec otNameBytes
        , interpOTID       = TrainerId (fromIntegral (rawG1OTID partyPokemon))
        , interpLevel      = level
        , interpMoves      = interpMoves
        , interpDVs        = dvs
        , interpStatExp    = promotedExp
        , interpExp        = rawG1Exp partyPokemon
        , interpStatus     = interpretStatus (rawG1Status partyPokemon)
        , interpCurrentHP  = fromIntegral (rawG1CurrentHP partyPokemon)
        , interpMaxHP      = fromIntegral (rawG1MaxHP partyPokemon)
        , interpAttack     = fromIntegral (rawG1Attack partyPokemon)
        , interpDefense    = fromIntegral (rawG1Defense partyPokemon)
        , interpSpeed      = fromIntegral (rawG1Speed partyPokemon)
        , interpSpecial    = Unified (fromIntegral (rawG1Special partyPokemon))
        , interpStatOrigin = StoredFromSave
        , interpGenFields  = InterpGen1Fields
            { interpCatchRate = rawG1CatchRate partyPokemon
            }
        }
     , speciesWarnings ++ listWarnings ++ moveWarnings ++ statWarnings
     )

interpretGen1BoxPokemon
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Map.Map MoveId Move
  -> TextCodec
  -> Int                              -- box number (1-based)
  -> Int                              -- slot index within the box
  -> InternalIndex                    -- species list byte
  -> RawGen1BoxPokemon                    -- struct data
  -> RawNamePair                      -- OT name and nickname bytes
  -> (InterpretedPokemon, [SaveWarning])
interpretGen1BoxPokemon indexMap speciesMap moveMap codec
                    boxNumber slotIndex listSpecies boxPokemon namePair =
  let otNameBytes = rawOTNameBytes namePair
      nickBytes   = rawNickBytes namePair
      structSpecies = rawG1BoxSpeciesIndex boxPokemon
      dvs           = unpackDVs (rawG1BoxDVBytes boxPokemon)
      level         = Level (fromIntegral (rawG1BoxBoxLevel boxPokemon))
      promotedExp   = promoteStatExp (rawG1BoxStatExp boxPokemon)

      -- Species resolution
      context = BoxSlot boxNumber (slotIndex + 1)
      (resolvedSpecies, speciesWarnings) = resolveSpecies indexMap speciesMap context structSpecies

      -- Species list cross-check
      listByte   = unInternalIndex listSpecies
      structByte = unInternalIndex structSpecies
      listWarnings
        | listByte == structByte = []
        | otherwise = [SpeciesListMismatch context listByte structByte]

      -- Move resolution
      rawMoveBytes = [rawG1BoxMove1 boxPokemon, rawG1BoxMove2 boxPokemon,
                      rawG1BoxMove3 boxPokemon, rawG1BoxMove4 boxPokemon]
      (resolvedMoves, moveWarnings) = resolveMoves moveMap context rawMoveBytes

      -- Compute stats from base stats + DVs + stat exp + level
      calculatedStats = case resolvedSpecies of
        KnownSpecies _ species -> Just (calcAllStats species dvs promotedExp level)
        _                      -> Nothing

  in ( InterpretedPokemon
        { interpSpecies    = resolvedSpecies
        , interpNickname   = decodeText codec nickBytes
        , interpOTName     = decodeText codec otNameBytes
        , interpOTID       = TrainerId (fromIntegral (rawG1BoxOTID boxPokemon))
        , interpLevel      = level
        , interpMoves      = resolvedMoves
        , interpDVs        = dvs
        , interpStatExp    = promotedExp
        , interpExp        = rawG1BoxExp boxPokemon
        , interpStatus     = interpretStatus (rawG1BoxStatus boxPokemon)
        , interpCurrentHP  = fromIntegral (rawG1BoxCurrentHP boxPokemon)
        , interpMaxHP      = maybe 0 statHP calculatedStats
        , interpAttack     = maybe 0 statAttack calculatedStats
        , interpDefense    = maybe 0 statDefense calculatedStats
        , interpSpeed      = maybe 0 statSpeed calculatedStats
        , interpSpecial    = maybe (Unified 0) statSpecial calculatedStats
        , interpStatOrigin = ComputedFromBase
        , interpGenFields  = InterpGen1Fields
            { interpCatchRate = rawG1BoxCatchRate boxPokemon
            }
        }
     , speciesWarnings ++ listWarnings ++ moveWarnings
     )


-- ── Resolution Helpers ───────────────────────────────────────

resolveSpecies
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> WarningContext
  -> InternalIndex
  -> (InterpretedSpecies, [SaveWarning])
resolveSpecies indexMap speciesMap context internalIdx =
  case Map.lookup internalIdx indexMap of
    Nothing  -> (UnknownSpecies internalIdx, [UnknownSpeciesIndex context internalIdx])
    Just dex -> case Map.lookup dex speciesMap of
      Nothing      -> (UnknownSpecies internalIdx, [UnknownSpeciesIndex context internalIdx])
      Just species -> (KnownSpecies dex species, [])

resolveMoves
  :: Map.Map MoveId Move
  -> WarningContext
  -> [Word8]
  -> ([InterpretedMove], [SaveWarning])
resolveMoves moveMap context rawBytes =
  let results = zipWith (resolveOneMove moveMap context) [1 ..] rawBytes
  in (map fst results, concatMap snd results)

resolveOneMove
  :: Map.Map MoveId Move
  -> WarningContext
  -> Int           -- move slot (1-4)
  -> Word8         -- raw move byte
  -> (InterpretedMove, [SaveWarning])
resolveOneMove _moveMap _context _moveSlot 0x00 = (EmptyMove, [])
resolveOneMove moveMap context moveSlot rawByte =
  let moveId = MoveId (fromIntegral rawByte)
  in case Map.lookup moveId moveMap of
    Just move -> (KnownMove moveId move, [])
    Nothing   -> (UnknownMove rawByte, [UnknownMoveId context moveSlot rawByte])

promoteStatExp :: RawStatExp -> StatExp
promoteStatExp raw = StatExp
  { expHP      = fromIntegral (rawExpHP raw)
  , expAttack  = fromIntegral (rawExpAttack raw)
  , expDefense = fromIntegral (rawExpDefense raw)
  , expSpeed   = fromIntegral (rawExpSpeed raw)
  , expSpecial = fromIntegral (rawExpSpecial raw)
  }


-- ── Decoders ───────────────────────────────────────────────────

-- | Decode a big-endian Binary-Coded Decimal byte string.
-- Each nybble is one decimal digit.
decodeBCD :: ByteString -> Int
decodeBCD = ByteString.foldl' addByte 0
  where
    addByte acc byte =
      acc * 100 + fromIntegral (byte `shiftR` 4) * 10 + fromIntegral (byte .&. 0x0F)

-- | Decode a bit-packed Pokédex byte string into the set of flagged
-- dex numbers. Bit N (0-indexed, LSB-first within each byte) maps
-- to species N+1.
decodePokedexFlags :: ByteString -> Set DexNumber
decodePokedexFlags bytes = Set.fromList
  [ DexNumber (bitIndex + 1)
  | (byteIndex, byte) <- zip [0 ..] (ByteString.unpack bytes)
  , bitOffset <- [0 .. 7]
  , let bitIndex = byteIndex * 8 + bitOffset
  , testBit byte bitOffset
  ]

resolveItems
  :: Map.Map ItemId Text
  -> Map.Map Machine MoveId
  -> Map.Map MoveId Move
  -> [RawItemEntry]
  -> [InventoryEntry]
resolveItems itemMap machineMap moveMap = map resolveEntry
  where
    resolveEntry entry =
      let itemByte = rawItemId entry
          itemId = ItemId (fromIntegral itemByte)
          itemName = case Map.lookup itemId itemMap of
            Just name -> name
            Nothing   -> resolveMachineItem machineMap moveMap itemByte
      in InventoryEntry { entryName = itemName, entryQuantity = fromIntegral (rawItemQuantity entry) }

-- | Try to resolve an item byte as a TM/HM. Falls back to "Unknown"
-- for bytes outside the machine range.
resolveMachineItem :: Map.Map Machine MoveId -> Map.Map MoveId Move -> Word8 -> Text
resolveMachineItem machineMap moveMap byte
  | byte >= 0xC4, byte <= 0xC8 =
      let machineNumber = fromIntegral byte - 0xC4 + 1
      in formatMachine "HM" machineNumber (HM (MachineNumber machineNumber))
  | byte >= 0xC9, byte <= 0xFA =
      let machineNumber = fromIntegral byte - 0xC9 + 1
      in formatMachine "TM" machineNumber (TM (MachineNumber machineNumber))
  | otherwise = Text.pack ("Unknown [0x" ++ showHexByte byte ++ "]")
  where
    formatMachine :: Text -> Int -> Machine -> Text
    formatMachine prefix machineNumber machine =
      let label = prefix <> Text.pack (padMachineNumber machineNumber)
      in case Map.lookup machine machineMap >>= (`Map.lookup` moveMap) of
          Just move -> label <> " \xFF0F " <> moveName move
          Nothing   -> label

    padMachineNumber :: Int -> String
    padMachineNumber machineNumber
      | machineNumber < 10    = "0" ++ show machineNumber
      | otherwise = show machineNumber

resolveDaycare
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> TextCodec
  -> RawDaycare
  -> (Maybe InterpretedDaycare, [SaveWarning])
resolveDaycare indexMap speciesMap codec daycare
  | rawDaycareInUse daycare == 0 = (Nothing, [])
  | otherwise =
      let (species, warnings) = resolveSpecies indexMap speciesMap DaycareSlot (rawDaycarePokemon daycare)
      in ( Just InterpretedDaycare
            { daycareSpecies  = species
            , daycareNickname = decodeText codec (rawDaycareNickname daycare)
            , daycareOTName   = decodeText codec (rawDaycareOTName daycare)
            }
         , warnings
         )

promotePlayTime :: RawPlayTime -> PlayTime
promotePlayTime raw = PlayTime
  { playHours   = fromIntegral (rawPlayHours raw)
  , playMinutes = fromIntegral (rawPlayMinutes raw)
  , playSeconds = fromIntegral (rawPlaySeconds raw)
  }

interpretOptions :: GameVariant -> Word8 -> InterpretedOptions
interpretOptions variant byte = InterpretedOptions
  { optTextSpeed       = case byte .&. 0x07 of
      1 -> TextFast
      3 -> TextMedium
      5 -> TextSlow
      n -> TextSpeedUnknown (fromIntegral n)
  , optBattleAnimation = if testBit byte 7 then AnimationsOff else AnimationsOn
  , optBattleStyle     = if testBit byte 6 then BattleShift else BattleSet
  , optSound           = yellowOnly variant $ case (byte .&. 0x30) `shiftR` 4 of
      0 -> Mono
      1 -> Earphone1
      2 -> Earphone2
      _ -> Earphone3
  }

yellowOnly :: GameVariant -> a -> Maybe a
yellowOnly Yellow value = Just value
yellowOnly _      _     = Nothing

-- | Decode a 2-byte little-endian Binary-Coded Decimal value.
-- Low byte first, high byte second. Each nybble is one decimal digit.
decodeBCDLE :: ByteString -> Int
decodeBCDLE bytes =
  let low  = ByteString.index bytes 0
      high = ByteString.index bytes 1
      decodeByte byte = fromIntegral (byte `shiftR` 4) * 10 + fromIntegral (byte .&. 0x0F)
  in decodeByte low + decodeByte high * 100

resolveFossil
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Map.Map ItemId Text
  -> RawFossilState
  -> (Maybe Text, Maybe InterpretedSpecies, [SaveWarning])
resolveFossil indexMap speciesMap itemMap fossil
  | rawFossilItemGiven fossil == 0 = (Nothing, Nothing, [])
  | otherwise =
      let itemByte = rawFossilItemGiven fossil
          itemName = case Map.lookup (ItemId (fromIntegral itemByte)) itemMap of
            Just name -> name
            Nothing   -> Text.pack ("Unknown [0x" ++ showHexByte itemByte ++ "]")
          speciesByte = ByteString.index (rawFossilResult fossil) 0
          (resolvedSpecies, speciesWarnings)
            | speciesByte == 0 = (Nothing, [])
            | otherwise =
                let (species, warnings) =
                      resolveSpecies indexMap speciesMap FossilSlot (InternalIndex speciesByte)
                in (Just species, warnings)
      in (Just itemName, resolvedSpecies, speciesWarnings)

promoteTransient :: RawTransientState -> InterpretedTransient
promoteTransient raw = InterpretedTransient
  { transLetterDelay        = fromIntegral (rawLetterDelay raw)
  , transMusicId            = fromIntegral (rawMusicId raw)
  , transMusicBank          = fromIntegral (rawMusicBank raw)
  , transContrastId         = fromIntegral (rawContrastId raw)
  , transEnemyTrainerClass  = fromIntegral (rawEnemyTrainerClass raw)
  , transBoulderSpriteIndex = fromIntegral (rawBoulderSpriteIndex raw)
  , transDungeonWarpDest    = fromIntegral (rawDungeonWarpDest raw)
  , transDungeonWarpUsed    = fromIntegral (rawDungeonWarpUsed raw)
  , transWarpedFromWarp     = fromIntegral (rawWarpedFromWarp raw)
  , transWarpedFromMap      = fromIntegral (rawWarpedFromMap raw)
  , transCardKeyDoorY       = fromIntegral (rawCardKeyDoorY raw)
  , transCardKeyDoorX       = fromIntegral (rawCardKeyDoorX raw)
  , transTrashCanLock1      = fromIntegral (rawTrashCanLock1 raw)
  , transTrashCanLock2      = fromIntegral (rawTrashCanLock2 raw)
  , transCurrentMapScript   = fromIntegral (rawCurrentMapScript raw)
  }


-- ── Hall of Fame Interpretation ──────────────────────────────

interpretHoFRecord
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> TextCodec
  -> Int                              -- record index (1-based)
  -> RawGen1HoFRecord
  -> (InterpretedHoFRecord, [SaveWarning])
interpretHoFRecord indexMap speciesMap codec recordIndex record =
  let results = zipWith
        (interpretHoFEntry indexMap speciesMap codec recordIndex)
        [1 ..] (rawGen1HoFEntries record)
      (maybeEntries, warningLists) = unzip results
      entries = catMaybes maybeEntries
  in (InterpretedHoFRecord { hofEntries = entries }, concat warningLists)

interpretHoFEntry
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> TextCodec
  -> Int                              -- record index (1-based)
  -> Int                              -- entry index (1-based)
  -> RawGen1HoFEntry
  -> (Maybe InterpretedHoFEntry, [SaveWarning])
interpretHoFEntry indexMap speciesMap codec recordIndex entryIndex entry
  | unInternalIndex (rawGen1HoFSpecies entry) == 0x00 = (Nothing, [])
  | otherwise =
      let context = HoFSlot recordIndex entryIndex
          (species, warnings) = resolveSpecies indexMap speciesMap context (rawGen1HoFSpecies entry)
      in ( Just InterpretedHoFEntry
            { hofSpecies  = species
            , hofLevel    = Level (fromIntegral (rawGen1HoFLevel entry))
            , hofNickname = decodeText codec (rawGen1HoFNickname entry)
            }
         , warnings
         )


-- ── Stat Cross-Check ─────────────────────────────────────────

data StoredStats = StoredStats
  { storedMaxHP   :: !Int
  , storedAttack  :: !Int
  , storedDefense :: !Int
  , storedSpeed   :: !Int
  , storedSpecial :: !Special
  }

checkStats :: WarningContext -> Species -> DVs -> StatExp -> Level -> StoredStats -> [SaveWarning]
checkStats context species dvs statExp level stored =
  let calculated = calcAllStats species dvs statExp level
      checks =
        [ ("HP",      storedMaxHP stored,   statHP calculated)
        , ("Attack",  storedAttack stored,  statAttack calculated)
        , ("Defense", storedDefense stored, statDefense calculated)
        , ("Speed",   storedSpeed stored,   statSpeed calculated)
        ] ++ specialChecks (storedSpecial stored) (statSpecial calculated)
  in [ StatMismatch context name storedValue calculatedValue
     | (name, storedValue, calculatedValue) <- checks
     , storedValue /= calculatedValue
     ]

specialChecks :: Special -> Special -> [(Text, Int, Int)]
specialChecks (Unified storedValue) (Unified calculatedValue) =
  [("Special", storedValue, calculatedValue)]
specialChecks (Split storedAtk storedDef) (Split calcAtk calcDef) =
  [("Sp.Atk", storedAtk, calcAtk), ("Sp.Def", storedDef, calcDef)]
-- Mismatched variants shouldn't happen, but handle gracefully
specialChecks (Unified storedValue) (Split calcAtk _calcDef) =
  [("Special", storedValue, calcAtk)]
specialChecks (Split storedAtk _storedDef) (Unified calculatedValue) =
  [("Special", storedAtk, calculatedValue)]


-- ── Progress Interpretation ───────────────────────────────────

interpretProgress :: GameData -> RawGen1SaveFile -> (InterpretedProgress, [SaveWarning])
interpretProgress gameData rawSave =
  let indexMap    = gameInternalIndex (gameSpeciesGraph gameData)
      speciesMap  = gameSpecies (gameSpeciesGraph gameData)
      flagNames  = gameGen1FlagNames gameData
      progress   = rawGen1Progress rawSave

      (playerStarter, playerStarterWarnings) =
        resolveSpecies indexMap speciesMap PlayerStarter (rawPlayerStarter progress)
      gameVariant = layoutGame (rawGen1Layout rawSave)
      (rivalStarter, rivalStarterWarnings) = case gameVariant of
        Yellow ->
          let rawByte = unInternalIndex (rawRivalStarter progress)
          in case rawByte of
            0 -> (RivalEeveelution EeveelutionPending, [])
            1 -> (RivalEeveelution (EeveelutionKnown JolteonPath), [])
            2 -> (RivalEeveelution (EeveelutionKnown FlareonPath), [])
            3 -> (RivalEeveelution (EeveelutionKnown VaporeonPath), [])
            _ -> (RivalEeveelution (EeveelutionUnknown rawByte),
                  [UnexpectedEeveelution rawByte])
        _ ->
          let (species, warnings) =
                resolveSpecies indexMap speciesMap RivalStarterSlot (rawRivalStarter progress)
          in (RivalStarterSpecies species, warnings)

      movementMode = case rawMovementStatus progress of
        0 -> Walking
        1 -> Biking
        2 -> Surfing
        byte -> UnknownMovement byte

      badgeList = case flagNames of
        Nothing    -> []
        Just names -> decodeNamedBitFlags (badgeNames names)
                        (ByteString.singleton (rawGen1Badges rawSave))

      gymList = case flagNames of
        Nothing    -> []
        Just names -> decodeNamedBitFlags (gymLeaderNames names)
                        (ByteString.singleton (rawDefeatedGyms progress))

      townList = case flagNames of
        Nothing    -> []
        Just names -> decodeNamedBitFlags (townNames names)
                        (rawTownsVisited progress)

      eventFlagList = case flagNames of
        Nothing    -> []
        Just names -> decodeNamedBitFlags (eventFlagNames names) (rawEventFlags progress)

      toggleFlagList = case flagNames of
        Nothing    -> []
        Just names -> decodeNamedBitFlags (toggleFlagNames names) (rawToggleFlags progress)

      mapScriptList = case flagNames of
        Nothing    -> []
        Just names -> decodeMapScripts (mapScriptNames names) (rawMapScripts progress)

      tradeList = case flagNames of
        Nothing    -> []
        Just names ->
          let tradeNameMap = case gameVariant of
                Yellow -> tradeNamesYellow names
                _      -> tradeNamesRB names
          in decodeNamedBitFlags tradeNameMap (rawInGameTrades progress)

      varFlags1 = rawVarFlags1 progress
      varFlags4 = rawVarFlags4 progress
      varFlags7 = rawVarFlags7 progress
      varFlags8 = rawVarFlags8 progress

      starterWarnings = playerStarterWarnings ++ rivalStarterWarnings

  in ( InterpretedProgress
        { progPlayerStarter     = playerStarter
        , progRivalStarter      = rivalStarter
        , progBadges            = badgeList
        , progDefeatedGyms      = gymList
        , progTownsVisited      = townList
        , progMovementMode      = movementMode
        , progEventFlags        = eventFlagList
        , progToggleFlags       = toggleFlagList
        , progMapScripts        = mapScriptList
        , progReceivedOldRod    = testBit varFlags1 3
        , progReceivedGoodRod   = testBit varFlags1 4
        , progReceivedSuperRod  = testBit varFlags1 5
        , progReceivedLapras    = testBit varFlags4 0
        , progReceivedStarter   = testBit varFlags4 3
        , progHealedAtCenter    = testBit varFlags4 2
        , progTrades              = tradeList
        , progTestBattle          = testBit varFlags7 0
        , progPreventMusicChange  = testBit varFlags7 1
        , progTrainerWantsBattle  = testBit varFlags7 3
        , progUsedFly             = testBit varFlags7 7
        , progStandingOnDoor      = testBit varFlags8 0
        , progSteppingFromDoor    = testBit varFlags8 1
        , progStandingOnWarp      = testBit varFlags8 2
        , progJumpingLedge        = testBit varFlags8 6
        , progSpinning            = testBit varFlags8 7
        , progBeatenLorelei       = testBit (ByteString.index (rawDefeatedLorelei progress) 0) 1
        , progActiveBoxSynced     = rawGen1ActiveBoxSynced rawSave
        }
     , starterWarnings
     )


-- ── Progress Decoders ─────────────────────────────────────────

-- | Walk a bit-packed byte string and produce a FlagState for every named bit.
-- LSB-first within each byte, same layout as decodePokedexFlags.
decodeNamedBitFlags :: Map.Map Int Text -> ByteString -> [FlagState]
decodeNamedBitFlags nameMap bytes =
  [ FlagState { flagName = name, flagIsSet = isSet }
  | (bitIndex, name) <- Map.toAscList nameMap
  , let byteIndex = bitIndex `div` 8
        bitOffset = bitIndex `mod` 8
        isSet     = byteIndex < ByteString.length bytes
                 && testBit (ByteString.index bytes byteIndex) bitOffset
  ]

-- | Produce a MapScriptState for every named script offset, including step 0.
decodeMapScripts :: Map.Map Int Text -> ByteString -> [MapScriptState]
decodeMapScripts nameMap bytes =
  [ MapScriptState { scriptName = name, scriptStep = stepValue }
  | (offset, name) <- Map.toAscList nameMap
  , let stepValue
          | offset < ByteString.length bytes = fromIntegral (ByteString.index bytes offset)
          | otherwise                        = 0
  ]


