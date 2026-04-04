{-# LANGUAGE OverloadedStrings #-}

-- | Progress flag interpretation for Gen 1 saves.
--
-- Decodes starter resolution, movement mode, badges, event flags,
-- toggle flags, map scripts, trades, and miscellaneous flag bits.

module Cinnabar.Save.Interpret.Gen1.Progress
  ( interpretProgress
  ) where

import Data.Bits (testBit)
import qualified Data.ByteString as ByteString
import Cinnabar.Types
import Cinnabar.Save.Layout (CartridgeLayout (..), GameVariant (..))
import Cinnabar.Save.Raw (RawGen1SaveFile (..), RawProgressFlags (..))
import Cinnabar.Save.Interpret.Types
import Cinnabar.Save.Interpret.Decode (decodeNamedBitFlags, decodeMapScripts)
import Cinnabar.Save.Interpret.Gen1.Pokemon (resolveSpecies)


interpretProgress :: GameData -> RawGen1SaveFile -> WithWarnings InterpretedProgress
interpretProgress gameData rawSave =
  let indexMap    = gameInternalIndex (gameSpeciesGraph gameData)
      speciesMap  = gameSpecies (gameSpeciesGraph gameData)
      flagNames  = gameGen1FlagNames gameData
      progress   = rawGen1Progress rawSave

      WithWarnings playerStarter playerStarterWarnings =
        resolveSpecies indexMap speciesMap PlayerStarter (rawPlayerStarter progress)
      gameVariant = layoutGame (rawGen1Layout rawSave)
      WithWarnings rivalStarter rivalStarterWarnings = case gameVariant of
        Yellow ->
          let rawByte = unInternalIndex (rawRivalStarter progress)
          in case rawByte of
            0 -> WithWarnings (RivalEeveelution EeveelutionPending) []
            1 -> WithWarnings (RivalEeveelution (EeveelutionKnown JolteonPath)) []
            2 -> WithWarnings (RivalEeveelution (EeveelutionKnown FlareonPath)) []
            3 -> WithWarnings (RivalEeveelution (EeveelutionKnown VaporeonPath)) []
            _ -> WithWarnings (RivalEeveelution (EeveelutionUnknown rawByte))
                              [UnexpectedEeveelution rawByte]
        _ ->
          let WithWarnings species warnings =
                resolveSpecies indexMap speciesMap RivalStarterSlot (rawRivalStarter progress)
          in WithWarnings (RivalStarterSpecies species) warnings

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

  in WithWarnings
     ( InterpretedProgress
        { progPlayerStarter      = playerStarter
        , progRivalStarter       = rivalStarter
        , progBadges             = badgeList
        , progDefeatedGyms       = gymList
        , progTownsVisited       = townList
        , progMovementMode       = movementMode
        , progEventFlags         = eventFlagList
        , progToggleFlags        = toggleFlagList
        , progMapScripts         = mapScriptList
        , progReceivedOldRod     = testBit varFlags1 3
        , progReceivedGoodRod    = testBit varFlags1 4
        , progReceivedSuperRod   = testBit varFlags1 5
        , progReceivedLapras     = testBit varFlags4 0
        , progReceivedStarter    = testBit varFlags4 3
        , progHealedAtCenter     = testBit varFlags4 2
        , progTrades             = tradeList
        , progTestBattle         = testBit varFlags7 0
        , progPreventMusicChange = testBit varFlags7 1
        , progTrainerWantsBattle = testBit varFlags7 3
        , progUsedFly            = testBit varFlags7 7
        , progStandingOnDoor     = testBit varFlags8 0
        , progSteppingFromDoor   = testBit varFlags8 1
        , progStandingOnWarp     = testBit varFlags8 2
        , progJumpingLedge       = testBit varFlags8 6
        , progSpinning           = testBit varFlags8 7
        , progBeatenLorelei      = testBit (ByteString.index (rawDefeatedLorelei progress) 0) 1
        , progActiveBoxSynced    = rawGen1ActiveBoxSynced rawSave
        }
     )
     starterWarnings
