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
  , InterpretedGenFields (..)
  , StatOrigin (..)
  , InterpretedMon (..)
  , InterpretedBox (..)
  , InterpretedSave (..)

    -- * Decoded sub-records
  , InventoryEntry (..)
  , PlayTime (..)

    -- * Warnings
  , SaveWarning (..)

    -- * Interpretation
  , interpretGen1Save
  ) where

import Data.Bits (testBit, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.List (zip4)
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
  )
import Cinnabar.Save.Gen1.Raw (RawGen1PartyMon (..), RawGen1BoxMon (..), RawStatExp (..))


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

data InterpretedMon = InterpretedMon
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
  , interpBoxMons   :: ![InterpretedMon]
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
  , interpPikachuFriend :: !(Maybe Int)
  , interpDaycareSpecies :: !(Maybe InterpretedSpecies)
  , interpOptions       :: !Word8
  , interpParty         :: ![InterpretedMon]
  , interpPCBoxes       :: ![InterpretedBox]
  , interpActiveBoxNum  :: !Int
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

data SaveWarning
  = UnknownSpeciesIndex !Int !InternalIndex
  | UnknownMoveId !Int !Int !Word8
  | SpeciesListMismatch !Int !Word8 !Word8
  | ChecksumMismatch !Word8 !Word8
  | StatMismatch !Int !Text !Int !Int
  | BoxBankChecksumMismatch !Int !Word8 !Word8       -- bank index, stored, calculated
  | BoxChecksumMismatch !Int !Int !Word8 !Word8      -- bank index, box-within-bank index, stored, calculated
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
        (rawGen1PartyNicks party)

      indexedSlots = zip4
        [0 ..]
        (rawGen1PartySpecies party)
        (rawGen1PartyMons party)
        namePairs

      (interpretedMons, monWarnings) = unzip
        [ interpretGen1Mon indexMap speciesMap moveMap codec
            idx listSpec mon names
        | (idx, listSpec, mon, names) <- indexedSlots
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

      -- PC box interpretation (non-empty boxes only)
      (interpretedBoxes, boxMonWarnings) = unzip
        [ ( InterpretedBox { interpBoxNumber = boxNum, interpBoxMons = boxMons }
          , concat perMonWarnings
          )
        | (boxIdx, rawBox) <- zip [0 :: Int ..] (rawGen1PCBoxes rawSave)
        , let boxNum   = boxIdx + 1
              boxCount = fromIntegral (rawGen1BoxCount rawBox) :: Int
        , boxCount > 0
        , let boxNamePairs = zipWith RawNamePair
                (rawGen1BoxOTNames rawBox)
                (rawGen1BoxNicks rawBox)
              boxSlots = zip4
                [0 ..]
                (rawGen1BoxSpecies rawBox)
                (rawGen1BoxMons rawBox)
                boxNamePairs
              (boxMons, perMonWarnings) = unzip
                [ interpretGen1BoxMon indexMap speciesMap moveMap codec
                    idx listSpec mon names
                | (idx, listSpec, mon, names) <- boxSlots
                ]
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
      , interpPikachuFriend = resolvePikachuFriend gameVariant (rawGen1PikachuFriend rawSave)
      , interpDaycareSpecies = resolveDaycare indexMap speciesMap daycareRecord
      , interpOptions       = rawGen1Options rawSave
      , interpParty         = take partyCount interpretedMons
      , interpPCBoxes       = interpretedBoxes
      , interpActiveBoxNum  = currentBoxNumber
      , interpWarnings      = concat monWarnings ++ checksumWarnings
                           ++ concat boxMonWarnings ++ boxBankWarnings
      , interpRaw           = RawGen1Save rawSave
      }


-- ── Per-Mon Interpretation ───────────────────────────────────

data RawNamePair = RawNamePair
  { rawOTNameBytes :: !ByteString
  , rawNickBytes   :: !ByteString
  } deriving (Eq, Show)

interpretGen1Mon
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Map.Map MoveId Move
  -> TextCodec
  -> Int                              -- slot index
  -> InternalIndex                    -- species list byte
  -> RawGen1PartyMon                  -- struct data
  -> RawNamePair                      -- OT name and nickname bytes
  -> (InterpretedMon, [SaveWarning])
interpretGen1Mon indexMap speciesMap moveMap codec
                 slotIndex listSpecies partyMon namePair =
  let otNameBytes = rawOTNameBytes namePair
      nickBytes   = rawNickBytes namePair
      structSpecies = rawG1SpeciesIndex partyMon
      dvs           = unpackDVs (rawG1DVBytes partyMon)
      level         = Level (fromIntegral (rawG1Level partyMon))
      promotedExp   = promoteStatExp (rawG1StatExp partyMon)

      -- Species resolution
      (interpSpecies, speciesWarnings) = resolveSpecies indexMap speciesMap slotIndex structSpecies

      -- Species list cross-check (compare raw bytes)
      listByte   = unInternalIndex listSpecies
      structByte = unInternalIndex structSpecies
      listWarnings
        | listByte == structByte = []
        | otherwise = [SpeciesListMismatch slotIndex listByte structByte]

      -- Move resolution
      rawMoveBytes = [rawG1Move1 partyMon, rawG1Move2 partyMon,
                      rawG1Move3 partyMon, rawG1Move4 partyMon]
      (interpMoves, moveWarnings) = resolveMoves moveMap slotIndex rawMoveBytes

      -- Stat cross-check
      storedStats = StoredStats
        { storedMaxHP   = fromIntegral (rawG1MaxHP partyMon)
        , storedAttack  = fromIntegral (rawG1Attack partyMon)
        , storedDefense = fromIntegral (rawG1Defense partyMon)
        , storedSpeed   = fromIntegral (rawG1Speed partyMon)
        , storedSpecial = Unified (fromIntegral (rawG1Special partyMon))
        }
      statWarnings = case interpSpecies of
        KnownSpecies _ species -> checkStats slotIndex species dvs promotedExp level storedStats
        _                      -> []

  in ( InterpretedMon
        { interpSpecies    = interpSpecies
        , interpNickname   = decodeText codec nickBytes
        , interpOTName     = decodeText codec otNameBytes
        , interpOTID       = TrainerId (fromIntegral (rawG1OTID partyMon))
        , interpLevel      = level
        , interpMoves      = interpMoves
        , interpDVs        = dvs
        , interpStatExp    = promotedExp
        , interpExp        = rawG1Exp partyMon
        , interpStatus     = rawG1Status partyMon
        , interpCurrentHP  = fromIntegral (rawG1CurrentHP partyMon)
        , interpMaxHP      = fromIntegral (rawG1MaxHP partyMon)
        , interpAttack     = fromIntegral (rawG1Attack partyMon)
        , interpDefense    = fromIntegral (rawG1Defense partyMon)
        , interpSpeed      = fromIntegral (rawG1Speed partyMon)
        , interpSpecial    = Unified (fromIntegral (rawG1Special partyMon))
        , interpStatOrigin = StoredFromSave
        , interpGenFields  = InterpGen1Fields
            { interpCatchRate = rawG1CatchRate partyMon
            }
        }
     , speciesWarnings ++ listWarnings ++ moveWarnings ++ statWarnings
     )

interpretGen1BoxMon
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Map.Map MoveId Move
  -> TextCodec
  -> Int                              -- slot index within the box
  -> InternalIndex                    -- species list byte
  -> RawGen1BoxMon                    -- struct data
  -> RawNamePair                      -- OT name and nickname bytes
  -> (InterpretedMon, [SaveWarning])
interpretGen1BoxMon indexMap speciesMap moveMap codec
                    slotIndex listSpecies boxMon namePair =
  let otNameBytes = rawOTNameBytes namePair
      nickBytes   = rawNickBytes namePair
      structSpecies = rawG1BoxSpeciesIndex boxMon
      dvs           = unpackDVs (rawG1BoxDVBytes boxMon)
      level         = Level (fromIntegral (rawG1BoxBoxLevel boxMon))
      promotedExp   = promoteStatExp (rawG1BoxStatExp boxMon)

      -- Species resolution
      (resolvedSpecies, speciesWarnings) = resolveSpecies indexMap speciesMap slotIndex structSpecies

      -- Species list cross-check
      listByte   = unInternalIndex listSpecies
      structByte = unInternalIndex structSpecies
      listWarnings
        | listByte == structByte = []
        | otherwise = [SpeciesListMismatch slotIndex listByte structByte]

      -- Move resolution
      rawMoveBytes = [rawG1BoxMove1 boxMon, rawG1BoxMove2 boxMon,
                      rawG1BoxMove3 boxMon, rawG1BoxMove4 boxMon]
      (resolvedMoves, moveWarnings) = resolveMoves moveMap slotIndex rawMoveBytes

      -- Compute stats from base stats + DVs + stat exp + level
      calculatedStats = case resolvedSpecies of
        KnownSpecies _ species -> Just (calcAllStats species dvs promotedExp level)
        _                      -> Nothing

  in ( InterpretedMon
        { interpSpecies    = resolvedSpecies
        , interpNickname   = decodeText codec nickBytes
        , interpOTName     = decodeText codec otNameBytes
        , interpOTID       = TrainerId (fromIntegral (rawG1BoxOTID boxMon))
        , interpLevel      = level
        , interpMoves      = resolvedMoves
        , interpDVs        = dvs
        , interpStatExp    = promotedExp
        , interpExp        = rawG1BoxExp boxMon
        , interpStatus     = rawG1BoxStatus boxMon
        , interpCurrentHP  = fromIntegral (rawG1BoxCurrentHP boxMon)
        , interpMaxHP      = maybe 0 statHP calculatedStats
        , interpAttack     = maybe 0 statAttack calculatedStats
        , interpDefense    = maybe 0 statDefense calculatedStats
        , interpSpeed      = maybe 0 statSpeed calculatedStats
        , interpSpecial    = maybe (Unified 0) statSpecial calculatedStats
        , interpStatOrigin = ComputedFromBase
        , interpGenFields  = InterpGen1Fields
            { interpCatchRate = rawG1BoxCatchRate boxMon
            }
        }
     , speciesWarnings ++ listWarnings ++ moveWarnings
     )


-- ── Resolution Helpers ───────────────────────────────────────

resolveSpecies
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Int
  -> InternalIndex
  -> (InterpretedSpecies, [SaveWarning])
resolveSpecies indexMap speciesMap slotIndex internalIdx =
  case Map.lookup internalIdx indexMap of
    Nothing  -> (UnknownSpecies internalIdx, [UnknownSpeciesIndex slotIndex internalIdx])
    Just dex -> case Map.lookup dex speciesMap of
      Nothing      -> (UnknownSpecies internalIdx, [UnknownSpeciesIndex slotIndex internalIdx])
      Just species -> (KnownSpecies dex species, [])

resolveMoves
  :: Map.Map MoveId Move
  -> Int
  -> [Word8]
  -> ([InterpretedMove], [SaveWarning])
resolveMoves moveMap slotIndex rawBytes =
  let results = zipWith (resolveOneMove moveMap slotIndex) [1 ..] rawBytes
  in (map fst results, concatMap snd results)

resolveOneMove
  :: Map.Map MoveId Move
  -> Int           -- party slot index
  -> Int           -- move slot (1-4)
  -> Word8         -- raw move byte
  -> (InterpretedMove, [SaveWarning])
resolveOneMove _moveMap _slotIndex _moveSlot 0x00 = (EmptyMove, [])
resolveOneMove moveMap slotIndex moveSlot rawByte =
  let moveId = MoveId (fromIntegral rawByte)
  in case Map.lookup moveId moveMap of
    Just move -> (KnownMove moveId move, [])
    Nothing   -> (UnknownMove rawByte, [UnknownMoveId slotIndex moveSlot rawByte])

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
-- Bit order MSB→LSB: Boulder, Cascade, Thunder, Rainbow, Soul,
-- Marsh, Volcano, Earth.
decodeBadges :: Word8 -> [Text]
decodeBadges byte =
  [ name | (bitPosition, name) <- zip [7, 6 .. 0] badgeNames, testBit byte bitPosition ]
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
      let number = fromIntegral byte - 0xC4 + 1
      in formatMachine "HM" number (HM (MachineNumber number))
  | byte >= 0xC9, byte <= 0xFA =
      let number = fromIntegral byte - 0xC9 + 1
      in formatMachine "TM" number (TM (MachineNumber number))
  | otherwise = Text.pack ("Unknown [0x" ++ showHexByte byte ++ "]")
  where
    formatMachine :: Text -> Int -> Machine -> Text
    formatMachine prefix number machine =
      let label = prefix <> Text.pack (padMachineNumber number)
      in case Map.lookup machine machineMap >>= (`Map.lookup` moveMap) of
          Just move -> label <> " \xFF0F " <> moveName move
          Nothing   -> label

    padMachineNumber :: Int -> String
    padMachineNumber n
      | n < 10    = "0" ++ show n
      | otherwise = show n

resolveDaycare
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> RawDaycare
  -> Maybe InterpretedSpecies
resolveDaycare indexMap speciesMap daycare
  | rawDaycareInUse daycare == 0 = Nothing
  | otherwise = Just $ case Map.lookup (rawDaycareMon daycare) indexMap of
      Nothing  -> UnknownSpecies (rawDaycareMon daycare)
      Just dex -> case Map.lookup dex speciesMap of
        Nothing      -> UnknownDexSpecies dex
        Just species -> KnownSpecies dex species

promotePlayTime :: RawPlayTime -> PlayTime
promotePlayTime raw = PlayTime
  { playHours   = fromIntegral (rawPlayHours raw)
  , playMinutes = fromIntegral (rawPlayMinutes raw)
  , playSeconds = fromIntegral (rawPlaySeconds raw)
  }

resolvePikachuFriend :: GameVariant -> Word8 -> Maybe Int
resolvePikachuFriend Yellow byte = Just (fromIntegral byte)
resolvePikachuFriend _      _    = Nothing


-- ── Stat Cross-Check ─────────────────────────────────────────

data StoredStats = StoredStats
  { storedMaxHP   :: !Int
  , storedAttack  :: !Int
  , storedDefense :: !Int
  , storedSpeed   :: !Int
  , storedSpecial :: !Special
  }

checkStats :: Int -> Species -> DVs -> StatExp -> Level -> StoredStats -> [SaveWarning]
checkStats slotIndex species dvs statExp level stored =
  let calculated = calcAllStats species dvs statExp level
      checks =
        [ ("HP",      storedMaxHP stored,   statHP calculated)
        , ("Attack",  storedAttack stored,  statAttack calculated)
        , ("Defense", storedDefense stored, statDefense calculated)
        , ("Speed",   storedSpeed stored,   statSpeed calculated)
        ] ++ specialChecks (storedSpecial stored) (statSpecial calculated)
  in [ StatMismatch slotIndex name storedValue calculatedValue
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
