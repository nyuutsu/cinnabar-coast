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
  , InterpretedMon (..)
  , InterpretedSave (..)

    -- * Warnings
  , SaveWarning (..)

    -- * Interpretation
  , interpretGen1Save
  ) where

import Data.ByteString (ByteString)
import Data.List (zip4)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16)

import Cinnabar.Types
import Cinnabar.Stats (CalcStats (..), calcAllStats)
import Cinnabar.TextCodec (TextCodec, decodeText)
import Cinnabar.Save.Checksum (calculateGen1Checksum)
import Cinnabar.Save.Layout (CartridgeLayout (..), SaveOffsets (..), Gen1SaveOffsets (..))
import Cinnabar.Save.Raw (RawSaveFile (..), RawGen1SaveFile (..), RawGen1Party (..))
import Cinnabar.Save.Gen1.Raw (RawGen1PartyMon (..), RawStatExp (..))


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
  , interpGenFields  :: !InterpretedGenFields
  } deriving (Eq, Show)

data InterpretedSave = InterpretedSave
  { interpPlayerName :: !GameText
  , interpRivalName  :: !GameText
  , interpParty      :: ![InterpretedMon]
  , interpWarnings   :: ![SaveWarning]
  , interpRaw        :: !RawSaveFile
  }


-- ── Warnings ─────────────────────────────────────────────────

data SaveWarning
  = UnknownSpeciesIndex !Int !InternalIndex
  | UnknownMoveId !Int !Int !Word8
  | SpeciesListMismatch !Int !Word8 !Word8
  | ChecksumMismatch !Word8 !Word8
  | StatMismatch !Int !Text !Int !Int
  deriving (Eq, Show)


-- ── Gen 1 Interpretation ─────────────────────────────────────

interpretGen1Save :: GameData -> TextCodec -> RawGen1SaveFile -> InterpretedSave
interpretGen1Save gameData codec rawSave =
  let speciesGraph  = gameSpeciesGraph gameData
      lookupTables  = gameLookupTables gameData
      indexMap       = gameInternalIndex speciesGraph
      speciesMap     = gameSpecies speciesGraph
      moveMap        = gameMoves lookupTables
      party          = rawGen1Party rawSave
      partyCount     = fromIntegral (rawGen1PartyCount party)

      indexedSlots = zip4
        [0 ..]
        (rawGen1PartySpecies party)
        (rawGen1PartyMons party)
        (zip (rawGen1PartyOTNames party) (rawGen1PartyNicks party))

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

  in InterpretedSave
      { interpPlayerName = decodeText codec (rawGen1PlayerName rawSave)
      , interpRivalName  = decodeText codec (rawGen1RivalName rawSave)
      , interpParty      = take partyCount interpretedMons
      , interpWarnings   = concat monWarnings ++ checksumWarnings
      , interpRaw        = RawGen1Save rawSave
      }


-- ── Per-Mon Interpretation ───────────────────────────────────

interpretGen1Mon
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Map.Map MoveId Move
  -> TextCodec
  -> Int                              -- slot index
  -> InternalIndex                    -- species list byte
  -> RawGen1PartyMon                  -- struct data
  -> (ByteString, ByteString)         -- (OT name bytes, nickname bytes)
  -> (InterpretedMon, [SaveWarning])
interpretGen1Mon indexMap speciesMap moveMap codec
                 slotIndex listSpecies partyMon (otNameBytes, nickBytes) =
  let structSpecies = rawG1SpeciesIndex partyMon
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
        , interpGenFields  = InterpGen1Fields
            { interpCatchRate = rawG1CatchRate partyMon
            }
        }
     , speciesWarnings ++ listWarnings ++ moveWarnings ++ statWarnings
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
