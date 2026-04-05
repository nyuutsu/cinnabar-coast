{-# LANGUAGE OverloadedStrings #-}

-- | Per-Pokemon interpretation for Gen 1 saves.
--
-- Resolves species, moves, DVs, stat exp, and stats for both
-- party and box Pokemon. Includes stat cross-checking against
-- computed values.

module Cinnabar.Save.Interpret.Gen1.Pokemon
  ( RawNamePair (..)
  , interpretGen1Pokemon
  , interpretGen1BoxPokemon
  , resolveSpecies
  , resolveMoves
  , resolveOneMove
  , promoteStatExp
  , StoredStats (..)
  , checkStats
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Data.Map.Strict as Map

import Cinnabar.Types
import Cinnabar.Stats (CalcStats (..), calcAllStats)
import Cinnabar.TextCodec (TextCodec, decodeText)
import Cinnabar.Save.Interpret.Types
import Cinnabar.Save.Interpret.Decode (interpretStatus)
import Cinnabar.Save.Gen1.Raw (RawGen1PartyPokemon (..), RawGen1BoxPokemon (..), RawStatExp (..))


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
  -> RawGen1PartyPokemon              -- struct data
  -> RawNamePair                      -- OT name and nickname bytes
  -> WithWarnings InterpretedPokemon
interpretGen1Pokemon indexMap speciesMap moveMap codec
                 slotIndex listSpecies partyPokemon namePair =
  let base        = rawG1PartyBase partyPokemon
      otNameBytes = rawOTNameBytes namePair
      nickBytes   = rawNickBytes namePair
      structSpecies = rawG1BoxSpeciesIndex base
      dvs           = unpackDVs (rawG1BoxDVBytes base)
      level         = Level (fromIntegral (rawG1Level partyPokemon))
      promotedExp   = promoteStatExp (rawG1BoxStatExp base)

      -- Species resolution
      context = PartySlot (slotIndex + 1)
      WithWarnings { computedResult = interpSpeciesResult, encounteredWarnings = speciesWarnings } =
        resolveSpecies indexMap speciesMap context structSpecies

      -- Species list cross-check (compare raw bytes)
      listByte   = unInternalIndex listSpecies
      structByte = unInternalIndex structSpecies
      listWarnings
        | listByte == structByte = []
        | otherwise = [SpeciesListMismatch context listByte structByte]

      -- Move resolution
      rawMoveBytes = [rawG1BoxMove1 base, rawG1BoxMove2 base,
                      rawG1BoxMove3 base, rawG1BoxMove4 base]
      WithWarnings { computedResult = interpMovesResult, encounteredWarnings = moveWarnings } =
        resolveMoves moveMap context rawMoveBytes

      -- Stat cross-check
      storedStats = StoredStats
        { storedMaxHP   = StatValue (fromIntegral (rawG1MaxHP partyPokemon))
        , storedAttack  = StatValue (fromIntegral (rawG1Attack partyPokemon))
        , storedDefense = StatValue (fromIntegral (rawG1Defense partyPokemon))
        , storedSpeed   = StatValue (fromIntegral (rawG1Speed partyPokemon))
        , storedSpecial = Unified (StatValue (fromIntegral (rawG1Special partyPokemon)))
        }
      statWarnings = case interpSpeciesResult of
        KnownSpecies _ species -> checkStats context species dvs promotedExp level storedStats
        _                      -> []

  in WithWarnings
       { computedResult = InterpretedPokemon
          { interpSpecies    = interpSpeciesResult
          , interpNickname   = decodeText codec nickBytes
          , interpOTName     = decodeText codec otNameBytes
          , interpOTID       = TrainerId (fromIntegral (rawG1BoxOTID base))
          , interpLevel      = level
          , interpMoves      = interpMovesResult
          , interpDVs        = dvs
          , interpStatExp    = promotedExp
          , interpExp        = Experience (rawG1BoxExp base)
          , interpStatus     = interpretStatus (rawG1BoxStatus base)
          , interpCurrentHP  = StatValue (fromIntegral (rawG1BoxCurrentHP base))
          , interpMaxHP      = StatValue (fromIntegral (rawG1MaxHP partyPokemon))
          , interpAttack     = StatValue (fromIntegral (rawG1Attack partyPokemon))
          , interpDefense    = StatValue (fromIntegral (rawG1Defense partyPokemon))
          , interpSpeed      = StatValue (fromIntegral (rawG1Speed partyPokemon))
          , interpSpecial    = Unified (StatValue (fromIntegral (rawG1Special partyPokemon)))
          , interpStatOrigin = StoredFromSave
          , interpGenFields  = InterpGen1Fields
              { interpCatchRate = rawG1BoxCatchRate base
              }
          }
       , encounteredWarnings = speciesWarnings ++ listWarnings ++ moveWarnings ++ statWarnings
       }

interpretGen1BoxPokemon
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Map.Map MoveId Move
  -> TextCodec
  -> Int                              -- box number (1-based)
  -> Int                              -- slot index within the box
  -> InternalIndex                    -- species list byte
  -> RawGen1BoxPokemon                -- struct data
  -> RawNamePair                      -- OT name and nickname bytes
  -> WithWarnings InterpretedPokemon
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
      WithWarnings { computedResult = resolvedSpecies, encounteredWarnings = speciesWarnings } =
        resolveSpecies indexMap speciesMap context structSpecies

      -- Species list cross-check
      listByte   = unInternalIndex listSpecies
      structByte = unInternalIndex structSpecies
      listWarnings
        | listByte == structByte = []
        | otherwise = [SpeciesListMismatch context listByte structByte]

      -- Move resolution
      rawMoveBytes = [rawG1BoxMove1 boxPokemon, rawG1BoxMove2 boxPokemon,
                      rawG1BoxMove3 boxPokemon, rawG1BoxMove4 boxPokemon]
      WithWarnings { computedResult = resolvedMoves, encounteredWarnings = moveWarnings } =
        resolveMoves moveMap context rawMoveBytes

      -- Compute stats from base stats + DVs + stat exp + level
      calculatedStats = case resolvedSpecies of
        KnownSpecies _ species -> Just (calcAllStats species dvs promotedExp level)
        _                      -> Nothing

  in WithWarnings
       { computedResult = InterpretedPokemon
          { interpSpecies    = resolvedSpecies
          , interpNickname   = decodeText codec nickBytes
          , interpOTName     = decodeText codec otNameBytes
          , interpOTID       = TrainerId (fromIntegral (rawG1BoxOTID boxPokemon))
          , interpLevel      = level
          , interpMoves      = resolvedMoves
          , interpDVs        = dvs
          , interpStatExp    = promotedExp
          , interpExp        = Experience (rawG1BoxExp boxPokemon)
          , interpStatus     = interpretStatus (rawG1BoxStatus boxPokemon)
          , interpCurrentHP  = StatValue (fromIntegral (rawG1BoxCurrentHP boxPokemon))
          , interpMaxHP      = maybe (StatValue 0) statHP calculatedStats
          , interpAttack     = maybe (StatValue 0) statAttack calculatedStats
          , interpDefense    = maybe (StatValue 0) statDefense calculatedStats
          , interpSpeed      = maybe (StatValue 0) statSpeed calculatedStats
          , interpSpecial    = maybe (Unified (StatValue 0)) statSpecial calculatedStats
          , interpStatOrigin = ComputedFromBase
          , interpGenFields  = InterpGen1Fields
              { interpCatchRate = rawG1BoxCatchRate boxPokemon
              }
          }
       , encounteredWarnings = speciesWarnings ++ listWarnings ++ moveWarnings
       }


-- ── Resolution Helpers ─────────────────────────────────────

resolveSpecies
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> WarningContext
  -> InternalIndex
  -> WithWarnings InterpretedSpecies
resolveSpecies indexMap speciesMap context internalIdx =
  case Map.lookup internalIdx indexMap of
    Nothing  -> WithWarnings
      { computedResult = UnknownSpecies internalIdx
      , encounteredWarnings = [UnknownSpeciesIndex context internalIdx]
      }
    Just dex -> case Map.lookup dex speciesMap of
      Nothing      -> WithWarnings
        { computedResult = UnknownSpecies internalIdx
        , encounteredWarnings = [UnknownSpeciesIndex context internalIdx]
        }
      Just species -> WithWarnings
        { computedResult = KnownSpecies dex species
        , encounteredWarnings = []
        }

resolveMoves
  :: Map.Map MoveId Move
  -> WarningContext
  -> [Word8]
  -> WithWarnings [InterpretedMove]
resolveMoves moveMap context rawBytes =
  let results = zipWith (resolveOneMove moveMap context) [1 ..] rawBytes
      moves    = map computedResult results
      warnings = concatMap encounteredWarnings results
  in WithWarnings { computedResult = moves, encounteredWarnings = warnings }

resolveOneMove
  :: Map.Map MoveId Move
  -> WarningContext
  -> Int           -- move slot (1-4)
  -> Word8         -- raw move byte
  -> WithWarnings InterpretedMove
resolveOneMove _moveMap _context _moveSlot 0x00 =
  WithWarnings { computedResult = EmptyMove, encounteredWarnings = [] }
resolveOneMove moveMap context moveSlot rawByte =
  let moveId = MoveId (fromIntegral rawByte)
  in case Map.lookup moveId moveMap of
    Just move -> WithWarnings
      { computedResult = KnownMove moveId move
      , encounteredWarnings = []
      }
    Nothing -> WithWarnings
      { computedResult = UnknownMove rawByte
      , encounteredWarnings = [UnknownMoveId context moveSlot rawByte]
      }

promoteStatExp :: RawStatExp -> StatExp
promoteStatExp raw = StatExp
  { expHP      = StatExpPoints (fromIntegral (rawExpHP raw))
  , expAttack  = StatExpPoints (fromIntegral (rawExpAttack raw))
  , expDefense = StatExpPoints (fromIntegral (rawExpDefense raw))
  , expSpeed   = StatExpPoints (fromIntegral (rawExpSpeed raw))
  , expSpecial = StatExpPoints (fromIntegral (rawExpSpecial raw))
  }


-- ── Stat Cross-Check ───────────────────────────────────────

data StoredStats = StoredStats
  { storedMaxHP   :: !StatValue
  , storedAttack  :: !StatValue
  , storedDefense :: !StatValue
  , storedSpeed   :: !StatValue
  , storedSpecial :: !(Special StatValue)
  }

checkStats :: WarningContext -> Species -> DVs -> StatExp -> Level -> StoredStats -> [SaveWarning]
checkStats context species dvs statExp level stored =
  let calculated = calcAllStats species dvs statExp level
      mismatch name storedValue calculatedValue
        | storedValue /= calculatedValue = [StatMismatch context name storedValue calculatedValue]
        | otherwise                      = []
  in mismatch "HP"      (storedMaxHP stored)   (statHP calculated)
  ++ mismatch "Attack"  (storedAttack stored)  (statAttack calculated)
  ++ mismatch "Defense" (storedDefense stored) (statDefense calculated)
  ++ mismatch "Speed"   (storedSpeed stored)   (statSpeed calculated)
  ++ specialStatWarnings context (storedSpecial stored) (statSpecial calculated)

specialStatWarnings :: WarningContext -> Special StatValue -> Special StatValue -> [SaveWarning]
specialStatWarnings context (Unified storedValue) (Unified calculatedValue)
  | storedValue /= calculatedValue = [StatMismatch context "Special" storedValue calculatedValue]
  | otherwise                      = []
specialStatWarnings context (Split storedAtk storedDef) (Split calcAtk calcDef) =
  [ StatMismatch context "Sp.Atk" storedAtk calcAtk | storedAtk /= calcAtk ]
  ++ [ StatMismatch context "Sp.Def" storedDef calcDef | storedDef /= calcDef ]
-- Mismatched variants shouldn't happen, but handle gracefully
specialStatWarnings context (Unified storedValue) (Split calcAtk _calcDef)
  | storedValue /= calcAtk = [StatMismatch context "Special" storedValue calcAtk]
  | otherwise               = []
specialStatWarnings context (Split storedAtk _storedDef) (Unified calculatedValue)
  | storedAtk /= calculatedValue = [StatMismatch context "Special" storedAtk calculatedValue]
  | otherwise                     = []
