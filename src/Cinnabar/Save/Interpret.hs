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
  , InterpretedProgress (..)
  , InterpretedHoFEntry (..)
  , InterpretedDaycare (..)
  , InterpretedTransient (..)
  , InterpretedHoFRecord (..)
  , InterpretedSave (..)

    -- * Decoded sub-records
  , InventoryEntry (..)
  , PlayTime (..)

    -- * Warnings
  , WarningContext (..)
  , SaveWarning (..)

    -- * Interpretation
  , interpretGen1Save
  ) where

import Data.Bits (testBit, popCount, shiftR, (.&.))
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
import Cinnabar.Save.Checksum (calculateGen1Checksum)
import Cinnabar.Save.Layout
  ( CartridgeLayout (..), SaveOffsets (..), Gen1SaveOffsets (..), GameVariant (..)
  , BoxBankInfo (..)
  )
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
  , interpStatus     :: !Word8
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

data InterpretedProgress = InterpretedProgress
  { progPlayerStarter     :: !InterpretedSpecies
  , progRivalStarter      :: !RivalStarter
  , progDefeatedGyms      :: ![Text]
  , progTownsVisited      :: ![Text]
  , progMovementMode      :: !Text
  , progEventFlags        :: ![FlagState]
  , progToggleFlags       :: ![FlagState]
  , progMapScripts        :: ![MapScriptState]
  , progReceivedOldRod    :: !Bool
  , progReceivedGoodRod   :: !Bool
  , progReceivedSuperRod  :: !Bool
  , progReceivedLapras    :: !Bool
  , progReceivedStarter   :: !Bool
  , progHealedAtCenter    :: !Bool
  , progTradesCompleted   :: !Int
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

data InterpretedSave = InterpretedSave
  { interpPlayerName    :: !GameText
  , interpRivalName     :: !GameText
  , interpPlayerID      :: !TrainerId
  , interpMoney         :: !Int
  , interpCasinoCoins   :: !Int
  , interpBadges        :: ![Text]
  , interpPokedexOwned  :: !(Set DexNumber)
  , interpPokedexSeen   :: !(Set DexNumber)
  , interpBagItems      :: ![InventoryEntry]
  , interpBoxItems      :: ![InventoryEntry]
  , interpPlayTime      :: !PlayTime
  , interpPlayTimeMaxed :: !Bool
  , interpCurrentBox    :: !Int
  , interpHoFCount      :: !Int
  , interpPikachuFriend   :: !(Maybe Int)
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
  , interpOptions         :: !Word8
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
        | rawGen1ChecksumValid rawSave = []
        | otherwise =
            let stored = rawGen1Checksum rawSave
                calculated = case layoutOffsets (rawGen1Layout rawSave) of
                  Gen1Offsets offsets ->
                    calculateGen1Checksum (rawGen1Bytes rawSave)
                      (g1ChecksumStart offsets) (g1ChecksumEnd offsets)
                  Gen2Offsets _ -> stored
            in [ChecksumMismatch stored calculated]

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
      boxBankWarnings = case layoutOffsets (rawGen1Layout rawSave) of
        Gen2Offsets _ -> []
        Gen1Offsets offsets ->
          let bankInfos   = g1BoxBanks offsets
              bankResults = rawGen1BoxBankValid rawSave
              saveBytes   = rawGen1Bytes rawSave
          in concat
            [ bankWarning ++ perBoxWarnings
            | (bankIdx, bankInfo, validity) <- zip3 [0 :: Int ..] bankInfos bankResults
            , let bankWarning
                    | bankChecksumValid validity = []
                    | otherwise =
                        let stored     = ByteString.index saveBytes (bankAllChecksum bankInfo)
                            calculated = calculateGen1Checksum saveBytes
                                           (bankStartOffset bankInfo)
                                           (bankAllChecksum bankInfo - 1)
                        in [BoxBankChecksumMismatch bankIdx stored calculated]
                  perBoxWarnings =
                    [ BoxChecksumMismatch bankIdx boxInBankIdx
                        (ByteString.index saveBytes (bankBoxChecksums bankInfo + boxInBankIdx))
                        (calculateGen1Checksum saveBytes boxOffset
                           (boxOffset + bankBoxDataSize bankInfo - 1))
                    | (boxInBankIdx, valid) <- zip [0 :: Int ..] (boxChecksumsValid validity)
                    , not valid
                    , let boxOffset = bankStartOffset bankInfo
                                    + boxInBankIdx * bankBoxDataSize bankInfo
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
      , interpBadges        = decodeBadges (rawGen1Badges rawSave)
      , interpPokedexOwned  = decodePokedexFlags (rawGen1PokedexOwned rawSave)
      , interpPokedexSeen   = decodePokedexFlags (rawGen1PokedexSeen rawSave)
      , interpBagItems      = resolveItems itemMap machineMap moveMap (rawGen1BagItems rawSave)
      , interpBoxItems      = resolveItems itemMap machineMap moveMap (rawGen1BoxItems rawSave)
      , interpPlayTime      = promotePlayTime rawPlayTimeRecord
      , interpPlayTimeMaxed = rawPlayMaxed rawPlayTimeRecord /= 0
      , interpCurrentBox    = currentBoxNumber
      , interpHoFCount      = fromIntegral (rawGen1HoFCount rawSave)
      , interpPikachuFriend   = resolvePikachuFriend gameVariant (rawGen1PikachuFriend rawSave)
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
      , interpOptions         = rawGen1Options rawSave
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
        , interpStatus     = rawG1Status partyPokemon
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
        , interpStatus     = rawG1BoxStatus boxPokemon
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

-- | Decode the badge bitfield into a list of badge names.
-- Bit order LSB→MSB: Boulder, Cascade, Thunder, Rainbow, Soul,
-- Marsh, Volcano, Earth.
decodeBadges :: Word8 -> [Text]
decodeBadges byte =
  [ name | (bitPosition, name) <- zip [0 .. 7] badgeNames, testBit byte bitPosition ]
  where
    badgeNames =
      ["Boulder", "Cascade", "Thunder", "Rainbow", "Soul", "Marsh", "Volcano", "Earth"]

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

resolvePikachuFriend :: GameVariant -> Word8 -> Maybe Int
resolvePikachuFriend Yellow byte = Just (fromIntegral byte)
resolvePikachuFriend _      _    = Nothing

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

      defeatedGyms = decodeDefeatedGyms (rawDefeatedGyms progress)
      townsVisited = decodeTownsVisited (rawTownsVisited progress)

      movementMode = case rawMovementStatus progress of
        0 -> "Walking"
        1 -> "Biking"
        2 -> "Surfing"
        _ -> "Unknown"

      eventFlagList = case flagNames of
        Nothing    -> []
        Just names -> decodeNamedBitFlags (eventFlagNames names) (rawEventFlags progress)

      toggleFlagList = case flagNames of
        Nothing    -> []
        Just names -> decodeNamedBitFlags (toggleFlagNames names) (rawToggleFlags progress)

      mapScriptList = case flagNames of
        Nothing    -> []
        Just names -> decodeMapScripts (mapScriptNames names) (rawMapScripts progress)

      varFlags1 = rawVarFlags1 progress
      varFlags4 = rawVarFlags4 progress
      varFlags7 = rawVarFlags7 progress
      varFlags8 = rawVarFlags8 progress

      starterWarnings = playerStarterWarnings ++ rivalStarterWarnings

  in ( InterpretedProgress
        { progPlayerStarter     = playerStarter
        , progRivalStarter      = rivalStarter
        , progDefeatedGyms      = defeatedGyms
        , progTownsVisited      = townsVisited
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
        , progTradesCompleted     = popCount (rawInGameTrades progress)
        , progTestBattle          = testBit varFlags7 0
        , progPreventMusicChange  = testBit varFlags7 1
        , progTrainerWantsBattle  = testBit varFlags7 3
        , progUsedFly             = testBit varFlags7 7
        , progStandingOnDoor      = testBit varFlags8 0
        , progSteppingFromDoor    = testBit varFlags8 1
        , progStandingOnWarp      = testBit varFlags8 2
        , progJumpingLedge        = testBit varFlags8 6
        , progSpinning            = testBit varFlags8 7
        , progBeatenLorelei       = testBit (rawDefeatedLorelei progress) 1
        , progActiveBoxSynced     = checkActiveBoxSync rawSave
        }
     , starterWarnings
     )


-- ── Progress Decoders ─────────────────────────────────────────

-- | Decode gym defeat bitfield. Same LSB→MSB order as badges.
decodeDefeatedGyms :: Word8 -> [Text]
decodeDefeatedGyms byte =
  [ name | (bitPosition, name) <- zip [0 .. 7] gymLeaderNames, testBit byte bitPosition ]
  where
    gymLeaderNames =
      ["Brock", "Misty", "Lt. Surge", "Erika", "Koga", "Sabrina", "Blaine", "Giovanni"]

-- | Decode towns visited from a big-endian Word16.
-- The game stores bit N in byte (N `div` 8), bit (N `mod` 8).
-- readWord16BE places byte 0 in the high byte, so game bit N
-- maps to Word16 bit ((N + 8) `mod` 16).
decodeTownsVisited :: Word16 -> [Text]
decodeTownsVisited word =
  [ name
  | (gameBit, name) <- zip [0 :: Int ..] townNames
  , testBit word ((gameBit + 8) `mod` 16)
  ]
  where
    townNames =
      [ "Pallet Town", "Viridian City", "Pewter City", "Cerulean City"
      , "Lavender Town", "Vermilion City", "Celadon City", "Fuchsia City"
      , "Cinnabar Island", "Indigo Plateau", "Saffron City"
      ]

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


-- ── Active Box Sync Check ─────────────────────────────────────

-- | Compare the Bank 1 current box region against the corresponding
-- PC bank box region. If byte-identical, the active box is synced.
checkActiveBoxSync :: RawGen1SaveFile -> Bool
checkActiveBoxSync rawSave =
  case layoutOffsets (rawGen1Layout rawSave) of
    Gen2Offsets _ -> True
    Gen1Offsets offsets ->
      let bankInfos       = g1BoxBanks offsets
          boxIndex        = fromIntegral (rawGen1CurrentBoxNum rawSave .&. 0x7F)
          currentBoxStart = g1CurrentBox offsets
          bytes           = rawGen1Bytes rawSave
      in case bankInfos of
        [] -> True
        (firstBank : _) ->
          let boxDataSize      = bankBoxDataSize firstBank
              currentBoxRegion = sliceBytes currentBoxStart boxDataSize bytes
          in case findPCBoxOffset bankInfos boxIndex of
            Nothing        -> False
            Just pcBoxStart ->
              let pcBoxRegion = sliceBytes pcBoxStart boxDataSize bytes
              in currentBoxRegion == pcBoxRegion

sliceBytes :: Int -> Int -> ByteString -> ByteString
sliceBytes offset len bytes = ByteString.take len (ByteString.drop offset bytes)

findPCBoxOffset :: [BoxBankInfo] -> Int -> Maybe Int
findPCBoxOffset banks targetBox = searchBanks banks 0
  where
    searchBanks [] _ = Nothing
    searchBanks (bank : rest) baseBox
      | targetBox < baseBox + bankBoxCount bank =
          Just (bankStartOffset bank + (targetBox - baseBox) * bankBoxDataSize bank)
      | otherwise = searchBanks rest (baseBox + bankBoxCount bank)
