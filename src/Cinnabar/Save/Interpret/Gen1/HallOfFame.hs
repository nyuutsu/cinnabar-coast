-- | Hall of Fame interpretation for Gen 1 saves.

module Cinnabar.Save.Interpret.Gen1.HallOfFame
  ( interpretHoFRecord
  , interpretHoFEntry
  ) where

import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

import Cinnabar.Types
import Cinnabar.TextCodec (TextCodec, decodeText)
import Cinnabar.Save.Raw (RawGen1HoFEntry (..), RawGen1HoFRecord (..))
import Cinnabar.Save.Interpret.Types
import Cinnabar.Save.Interpret.Gen1.Pokemon (resolveSpecies)


interpretHoFRecord
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> TextCodec
  -> Int                              -- record index (1-based)
  -> RawGen1HoFRecord
  -> WithWarnings InterpretedHoFRecord
interpretHoFRecord indexMap speciesMap codec recordIndex record =
  let results = zipWith
        (interpretHoFEntry indexMap speciesMap codec recordIndex)
        [1 ..] (rawGen1HoFEntries record)
      entries  = catMaybes (map computedResult results)
      warnings = concatMap encounteredWarnings results
  in WithWarnings { computedResult = InterpretedHoFRecord { hofEntries = entries }, encounteredWarnings = warnings }

interpretHoFEntry
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> TextCodec
  -> Int                              -- record index (1-based)
  -> Int                              -- entry index (1-based)
  -> RawGen1HoFEntry
  -> WithWarnings (Maybe InterpretedHoFEntry)
interpretHoFEntry indexMap speciesMap codec recordIndex entryIndex entry
  | unInternalIndex (rawGen1HoFSpecies entry) == 0x00 =
      WithWarnings { computedResult = Nothing, encounteredWarnings = [] }
  | otherwise =
      let context = HoFSlot recordIndex entryIndex
          WithWarnings { computedResult = species, encounteredWarnings = warnings } =
            resolveSpecies indexMap speciesMap context (rawGen1HoFSpecies entry)
      in WithWarnings
           { computedResult = Just InterpretedHoFEntry
              { hofSpecies  = species
              , hofLevel    = Level (fromIntegral (rawGen1HoFLevel entry))
              , hofNickname = decodeText codec (rawGen1HoFNickname entry)
              }
           , encounteredWarnings = warnings
           }
