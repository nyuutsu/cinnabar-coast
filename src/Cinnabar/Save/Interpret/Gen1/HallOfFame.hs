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
