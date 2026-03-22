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
import Data.Text (Text)
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
  -> (InterpretedPokemon, [SaveWarning])
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
      (interpSpeciesResult, speciesWarnings) = resolveSpecies indexMap speciesMap context structSpecies

      -- Species list cross-check (compare raw bytes)
      listByte   = unInternalIndex listSpecies
      structByte = unInternalIndex structSpecies
      listWarnings
        | listByte == structByte = []
        | otherwise = [SpeciesListMismatch context listByte structByte]

      -- Move resolution
      rawMoveBytes = [rawG1BoxMove1 base, rawG1BoxMove2 base,
                      rawG1BoxMove3 base, rawG1BoxMove4 base]
      (interpMovesResult, moveWarnings) = resolveMoves moveMap context rawMoveBytes

      -- Stat cross-check
      storedStats = StoredStats
        { storedMaxHP   = fromIntegral (rawG1MaxHP partyPokemon)
        , storedAttack  = fromIntegral (rawG1Attack partyPokemon)
        , storedDefense = fromIntegral (rawG1Defense partyPokemon)
        , storedSpeed   = fromIntegral (rawG1Speed partyPokemon)
        , storedSpecial = Unified (fromIntegral (rawG1Special partyPokemon))
        }
      statWarnings = case interpSpeciesResult of
        KnownSpecies _ species -> checkStats context species dvs promotedExp level storedStats
        _                      -> []

  in ( InterpretedPokemon
        { interpSpecies    = interpSpeciesResult
        , interpNickname   = decodeText codec nickBytes
        , interpOTName     = decodeText codec otNameBytes
        , interpOTID       = TrainerId (fromIntegral (rawG1BoxOTID base))
        , interpLevel      = level
        , interpMoves      = interpMovesResult
        , interpDVs        = dvs
        , interpStatExp    = promotedExp
        , interpExp        = rawG1BoxExp base
        , interpStatus     = interpretStatus (rawG1BoxStatus base)
        , interpCurrentHP  = fromIntegral (rawG1BoxCurrentHP base)
        , interpMaxHP      = fromIntegral (rawG1MaxHP partyPokemon)
        , interpAttack     = fromIntegral (rawG1Attack partyPokemon)
        , interpDefense    = fromIntegral (rawG1Defense partyPokemon)
        , interpSpeed      = fromIntegral (rawG1Speed partyPokemon)
        , interpSpecial    = Unified (fromIntegral (rawG1Special partyPokemon))
        , interpStatOrigin = StoredFromSave
        , interpGenFields  = InterpGen1Fields
            { interpCatchRate = rawG1BoxCatchRate base
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
  -> RawGen1BoxPokemon                -- struct data
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


-- ── Resolution Helpers ─────────────────────────────────────

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


-- ── Stat Cross-Check ───────────────────────────────────────

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
