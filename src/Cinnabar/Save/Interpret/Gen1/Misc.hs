{-# LANGUAGE OverloadedStrings #-}

-- | Miscellaneous Gen 1 interpretation helpers.
--
-- Daycare, fossil resolution, play time promotion, and
-- transient state promotion.

module Cinnabar.Save.Interpret.Gen1.Misc
  ( resolveDaycare
  , resolveFossil
  , promotePlayTime
  , promoteTransient
  ) where

import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import Cinnabar.Types
import Cinnabar.TextCodec (TextCodec, decodeText, showHexByte)
import Cinnabar.Save.Raw (RawDaycare (..), RawFossilState (..), RawPlayTime (..), RawTransientState (..))
import Cinnabar.Save.Interpret.Types
import Cinnabar.Save.Interpret.Gen1.Pokemon (resolveSpecies)


resolveDaycare
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> TextCodec
  -> RawDaycare
  -> WithWarnings (Maybe InterpretedDaycare)
resolveDaycare indexMap speciesMap codec daycare
  | rawDaycareInUse daycare == 0 = WithWarnings Nothing []
  | otherwise =
      let WithWarnings species warnings =
            resolveSpecies indexMap speciesMap DaycareSlot (rawDaycarePokemon daycare)
      in WithWarnings
           ( Just InterpretedDaycare
              { daycareSpecies  = species
              , daycareNickname = decodeText codec (rawDaycareNickname daycare)
              , daycareOTName   = decodeText codec (rawDaycareOTName daycare)
              }
           )
           warnings

resolveFossil
  :: Map.Map InternalIndex DexNumber
  -> Map.Map DexNumber Species
  -> Map.Map ItemId Text
  -> RawFossilState
  -> WithWarnings FossilResult
resolveFossil indexMap speciesMap itemMap fossil
  | rawFossilItemGiven fossil == 0 =
      WithWarnings (FossilResult Nothing Nothing) []
  | otherwise =
      let itemByte = rawFossilItemGiven fossil
          itemName = case Map.lookup (ItemId (fromIntegral itemByte)) itemMap of
            Just name -> name
            Nothing   -> Text.pack ("Unknown [0x" ++ showHexByte itemByte ++ "]")
          speciesByte = ByteString.index (rawFossilResult fossil) 0
          WithWarnings resolvedSpecies speciesWarnings
            | speciesByte == 0 = WithWarnings Nothing []
            | otherwise =
                let WithWarnings species warnings =
                      resolveSpecies indexMap speciesMap FossilSlot (InternalIndex speciesByte)
                in WithWarnings (Just species) warnings
      in WithWarnings (FossilResult (Just itemName) resolvedSpecies) speciesWarnings

promotePlayTime :: RawPlayTime -> PlayTime
promotePlayTime raw = PlayTime
  { playHours   = fromIntegral (rawPlayHours raw)
  , playMinutes = fromIntegral (rawPlayMinutes raw)
  , playSeconds = fromIntegral (rawPlaySeconds raw)
  }

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
