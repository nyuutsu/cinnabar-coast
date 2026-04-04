{-# LANGUAGE OverloadedStrings #-}

-- | Gen 1 save interpretation orchestrator.
--
-- Top-level function that wires together all Gen 1 sub-modules
-- to produce an InterpretedSave from a RawGen1SaveFile.

module Cinnabar.Save.Interpret.Gen1
  ( interpretGen1Save
  ) where

import Data.Bits ((.&.))
import Data.List (zip4)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text

import Cinnabar.Types
import Cinnabar.TextCodec (TextCodec, decodeText)
import Cinnabar.Save.Layout
  ( CartridgeLayout (..), BoxCapacity (..)
  , gen1PartyCapacity, gen1BagCapacity, gen1PCItemCapacity
  , gen1HoFRecordCount
  )
import Cinnabar.Save.Raw
  ( RawSaveFile (..), RawGen1SaveFile (..), RawGen1Party (..)
  , RawGen1Box (..), RawBankValidity (..), ChecksumPair (..)
  , RawPlayTime (..), RawPlayerPosition (..), RawProgressFlags (..)
  , RawSafariState (..)
  )
import Cinnabar.Save.Interpret.Types
import Cinnabar.Save.Interpret.Decode
  ( decodeBCD, decodeBCDLE, decodePokedexFlags
  , interpretOptions, yellowOnly
  )
import Cinnabar.Save.Interpret.Gen1.Pokemon
  ( RawNamePair (..)
  , interpretGen1Pokemon, interpretGen1BoxPokemon
  )
import Cinnabar.Save.Interpret.Gen1.Progress (interpretProgress)
import Cinnabar.Save.Interpret.Gen1.Items (resolveItems)
import Cinnabar.Save.Interpret.Gen1.HallOfFame (interpretHoFRecord)
import Cinnabar.Save.Interpret.Gen1.Misc
  ( resolveDaycare, resolveFossil, promotePlayTime, promoteTransient
  )


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

      pokemonResults =
        [ interpretGen1Pokemon indexMap speciesMap moveMap codec
            idx listSpec pokemon names
        | (idx, listSpec, pokemon, names) <- indexedSlots
        ]
      interpretedMembers = map computedResult pokemonResults
      pokemonWarnings    = map encounteredWarnings pokemonResults

      checksumWarnings
        | rawGen1Checksum rawSave == rawGen1CalculatedChecksum rawSave = []
        | otherwise = [ChecksumMismatch (rawGen1Checksum rawSave)
                                        (rawGen1CalculatedChecksum rawSave)]

      currentBoxNumber = fromIntegral (rawGen1CurrentBoxNum rawSave .&. 0x7F) + 1

      rawPlayTimeRecord = rawGen1PlayTime rawSave
      daycareRecord     = rawGen1Daycare rawSave
      WithWarnings interpretedDaycare daycareWarnings =
        resolveDaycare indexMap speciesMap codec daycareRecord

      -- Player position
      positionRecord = rawGen1PlayerPosition rawSave

      -- Safari state
      safariRecord = rawGen1Safari rawSave

      -- Fossil resolution
      fossilRecord = rawGen1Fossil rawSave
      WithWarnings fossilResult fossilWarnings =
        resolveFossil indexMap speciesMap itemMap fossilRecord

      -- Transient state
      transientRecord = rawGen1Transient rawSave

      -- PC box interpretation (non-empty boxes only)
      (interpretedBoxes, boxPokemonWarnings) = unzip
        [ ( InterpretedBox { interpBoxNumber = boxNum, interpBoxMembers = boxMembers }
          , concatMap encounteredWarnings boxResults
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
              boxResults =
                [ interpretGen1BoxPokemon indexMap speciesMap moveMap codec
                    boxNum idx listSpec pokemon names
                | (idx, listSpec, pokemon, names) <- boxSlots
                ]
              boxMembers = map computedResult boxResults
        ]

      -- Hall of Fame interpretation
      hofCount = fromIntegral (rawGen1HoFCount rawSave) :: Int
      hofResults =
        [ interpretHoFRecord indexMap speciesMap codec recordIndex record
        | (recordIndex, record) <- zip [1 ..] (take hofCount (rawGen1HallOfFame rawSave))
        ]
      interpretedHoF = map computedResult hofResults
      hofWarnings    = map encounteredWarnings hofResults

      -- Box bank checksum warnings
      boxBankWarnings = concat
        [ bankWarning ++ perBoxWarnings
        | (bankIdx, validity) <- zip [0 :: Int ..] (rawGen1BoxBankValid rawSave)
        , let bankWarning
                | bankStoredChecksum validity == bankCalculatedChecksum validity = []
                | otherwise = [BoxBankChecksumMismatch bankIdx
                    (bankStoredChecksum validity) (bankCalculatedChecksum validity)]
              perBoxWarnings =
                [ BoxChecksumMismatch bankIdx boxInBankIdx
                    (checksumStored pair) (checksumCalculated pair)
                | (boxInBankIdx, pair) <-
                    zip [0 :: Int ..] (boxChecksumPairs validity)
                , checksumStored pair /= checksumCalculated pair
                ]
        ]

      -- Progress interpretation
      WithWarnings progress progressSpeciesWarnings = interpretProgress gameData rawSave

      progressWarnings
        | progActiveBoxSynced progress = []
        | otherwise = [ActiveBoxDesync]

      -- Count-exceeds-capacity warnings
      boxCapacity = unBoxCapacity (layoutBoxCapacity (rawGen1Layout rawSave))
      checkCount fieldName rawCount capacity
        | rawCount > capacity = Just (CountExceedsCapacity fieldName rawCount capacity)
        | otherwise = Nothing
      countWarnings = catMaybes $
        [ checkCount "Party" (fromIntegral (rawGen1PartyCount party)) gen1PartyCapacity
        , checkCount "Bag items" (fromIntegral (rawGen1BagItemCount rawSave)) gen1BagCapacity
        , checkCount "PC items" (fromIntegral (rawGen1BoxItemCount rawSave)) gen1PCItemCapacity
        , checkCount "Hall of Fame" (fromIntegral (rawGen1HoFCount rawSave)) gen1HoFRecordCount
        , checkCount ("Current box " <> Text.pack (show currentBoxNumber))
            (fromIntegral (rawGen1BoxCount (rawGen1CurrentBox rawSave))) boxCapacity
        ] ++
        [ checkCount ("Box " <> Text.pack (show boxNum))
            (fromIntegral (rawGen1BoxCount rawBox)) boxCapacity
        | (boxIdx, rawBox) <- zip [0 :: Int ..] (rawGen1PCBoxes rawSave)
        , let boxNum = boxIdx + 1
        ]

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
      , interpFossilItem      = fossilItemName fossilResult
      , interpFossilResult    = fossilSpecies fossilResult
      , interpTransient       = promoteTransient transientRecord
      , interpOptions         = interpretOptions gameVariant (rawGen1Options rawSave)
      , interpParty         = take partyCount interpretedMembers
      , interpPCBoxes       = interpretedBoxes
      , interpHallOfFame    = interpretedHoF
      , interpActiveBoxNum  = currentBoxNumber
      , interpProgress      = progress
      , interpWarnings      = countWarnings
                           ++ concat pokemonWarnings ++ checksumWarnings
                           ++ concat boxPokemonWarnings ++ boxBankWarnings
                           ++ concat hofWarnings ++ daycareWarnings
                           ++ fossilWarnings
                           ++ progressSpeciesWarnings ++ progressWarnings
      , interpRaw           = RawGen1Save rawSave
      }
