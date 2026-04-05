-- | Write path for Gen 1 save files.
--
-- Round-trip by construction: starts from the original ByteString,
-- patches only the fields that changed, recalculates the checksum.
-- Bytes we don't understand are preserved automatically.

module Cinnabar.Save.Serialize
  ( -- * Top-level serializer
    serializeGen1Save

    -- * Struct serializers
  , serializeGen1PartyPokemon
  , serializeGen1BoxPokemon
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word8)

import Cinnabar.Binary (SaveOffset (..), ByteEdit (..), writeByte, writeWord16BE, writeWord24BE, patchByte, patchBytes, patchSlots, applyPatches)
import Cinnabar.Save.Checksum (calculateGen1Checksum)
import Cinnabar.Save.Gen1.Raw (RawGen1PartyPokemon (..), RawGen1BoxPokemon (..), RawStatExp (..))
import Cinnabar.Save.Layout
  ( CartridgeLayout (..), NameLength (..), BoxCapacity (..)
  , SaveOffsets (..), Gen1SaveOffsets (..), BoxBankInfo (..)
  , gen1PartyCapacity, gen1PartyPokemonSize, gen1BoxPokemonSize
  , gen1HoFRecordCount, gen1HoFRecordSize, gen1HoFEntrySize
  )
import Cinnabar.Save.Raw
  ( RawGen1SaveFile (..), RawGen1Party (..), RawGen1Box (..)
  , RawItemEntry (..), RawPlayTime (..), RawDaycare (..), RawProgressFlags (..)
  , RawPlayerPosition (..), RawSafariState (..), RawFossilState (..)
  , RawTransientState (..)
  , RawGen1HoFEntry (..), RawGen1HoFRecord (..)
  )
import Cinnabar.Types (InternalIndex (..))


-- ── Top-Level Serializer ─────────────────────────────────────

serializeGen1Save :: RawGen1SaveFile -> ByteString
serializeGen1Save save = case layoutOffsets (rawGen1Layout save) of
  Gen2Offsets _ -> error "serializeGen1Save: Gen 2 offsets on a Gen 1 save"
  Gen1Offsets offsets ->
    let nameLen     = layoutNameLen (rawGen1Layout save)
        boxCapacity = layoutBoxCapacity (rawGen1Layout save)
        originalBytes = rawGen1Bytes save
        progress    = rawGen1Progress save
        position    = rawGen1PlayerPosition save
        safari      = rawGen1Safari save
        fossil      = rawGen1Fossil save
        transient   = rawGen1Transient save
        daycare     = rawGen1Daycare save

        partyRegionSize = 1 + (gen1PartyCapacity + 1)
                        + gen1PartyCapacity * gen1PartyPokemonSize
                        + gen1PartyCapacity * unNameLength nameLen * 2
        originalPartyRegion = ByteString.take partyRegionSize
                            $ ByteString.drop (unSaveOffset (g1PartyData offsets)) originalBytes

        boxRegionSize = 1 + (unBoxCapacity boxCapacity + 1)
                      + unBoxCapacity boxCapacity * gen1BoxPokemonSize
                      + unBoxCapacity boxCapacity * unNameLength nameLen * 2
        originalBoxRegion = ByteString.take boxRegionSize
                          $ ByteString.drop (unSaveOffset (g1CurrentBox offsets)) originalBytes

        hofRegionSize = gen1HoFRecordCount * gen1HoFRecordSize
        originalHoFRegion = ByteString.take hofRegionSize
                          $ ByteString.drop (unSaveOffset (g1HallOfFame offsets)) originalBytes

        patches =
          -- Main data regions
          [ ByteEdit { editOffset = g1HallOfFame offsets
                     , editBytes  = serializeHallOfFame nameLen originalHoFRegion (rawGen1HallOfFame save) }
          , ByteEdit { editOffset = g1PlayerName offsets,    editBytes = rawGen1PlayerName save }
          , ByteEdit { editOffset = g1RivalName offsets,     editBytes = rawGen1RivalName save }
          , ByteEdit { editOffset = g1PartyData offsets
                     , editBytes  = serializeGen1Party nameLen originalPartyRegion (rawGen1Party save) }
          , ByteEdit { editOffset = g1CurrentBox offsets
                     , editBytes  = serializeGen1Box nameLen boxCapacity originalBoxRegion
                                      (rawGen1CurrentBox save) }
          , ByteEdit { editOffset = g1PokedexOwned offsets,  editBytes = rawGen1PokedexOwned save }
          , ByteEdit { editOffset = g1PokedexSeen offsets,   editBytes = rawGen1PokedexSeen save }
          , ByteEdit { editOffset = g1BagItems offsets,      editBytes = serializeItemList (rawGen1BagItems save) }
          , ByteEdit { editOffset = g1BoxItems offsets,      editBytes = serializeItemList (rawGen1BoxItems save) }
          , ByteEdit { editOffset = g1Money offsets,         editBytes = rawGen1Money save }
          , ByteEdit { editOffset = g1CasinoCoins offsets,   editBytes = rawGen1CasinoCoins save }
          , ByteEdit { editOffset = g1Badges offsets,        editBytes = writeByte (rawGen1Badges save) }
          , ByteEdit { editOffset = g1PlayerID offsets,      editBytes = writeWord16BE (rawGen1PlayerID save) }
          , ByteEdit { editOffset = g1Options offsets,       editBytes = writeByte (rawGen1Options save) }
          , ByteEdit { editOffset = g1CurrentBoxNumber offsets, editBytes = writeByte (rawGen1CurrentBoxNum save) }
          , ByteEdit { editOffset = g1HoFCount offsets,      editBytes = writeByte (rawGen1HoFCount save) }
          , ByteEdit { editOffset = g1PlayTime offsets,      editBytes = serializeRawPlayTime (rawGen1PlayTime save) }
          , ByteEdit { editOffset = g1PikachuHappiness offsets, editBytes = writeByte (rawGen1PikachuHappiness save) }
          , ByteEdit { editOffset = g1PikachuMood offsets,   editBytes = writeByte (rawGen1PikachuMood save) }
          , ByteEdit { editOffset = g1SurfingHiScore offsets, editBytes = rawGen1SurfingHiScore save }
          , ByteEdit { editOffset = g1PrinterSettings offsets, editBytes = writeByte (rawGen1PrinterSettings save) }
          -- Daycare
          , ByteEdit { editOffset = g1DaycareInUse offsets,  editBytes = writeByte (rawDaycareInUse daycare) }
          , ByteEdit { editOffset = g1DaycarePokemon offsets, editBytes = writeByte (unInternalIndex (rawDaycarePokemon daycare)) }
          , ByteEdit { editOffset = g1DaycareNickname offsets, editBytes = rawDaycareNickname daycare }
          , ByteEdit { editOffset = g1DaycareOTName offsets, editBytes = rawDaycareOTName daycare }
          -- Progress flags
          , ByteEdit { editOffset = g1EventFlags offsets,    editBytes = rawEventFlags progress }
          , ByteEdit { editOffset = g1ToggleFlags offsets,   editBytes = rawToggleFlags progress }
          , ByteEdit { editOffset = g1MapScripts offsets,    editBytes = rawMapScripts progress }
          , ByteEdit { editOffset = g1DefeatedGyms offsets,  editBytes = writeByte (rawDefeatedGyms progress) }
          , ByteEdit { editOffset = g1PlayerStarter offsets,  editBytes = writeByte (unInternalIndex (rawPlayerStarter progress)) }
          , ByteEdit { editOffset = g1RivalStarter offsets,  editBytes = writeByte (unInternalIndex (rawRivalStarter progress)) }
          , ByteEdit { editOffset = g1TownsVisited offsets,  editBytes = rawTownsVisited progress }
          , ByteEdit { editOffset = g1MovementStatus offsets, editBytes = writeByte (rawMovementStatus progress) }
          , ByteEdit { editOffset = g1VarFlags1 offsets,     editBytes = writeByte (rawVarFlags1 progress) }
          , ByteEdit { editOffset = g1VarFlags2 offsets,     editBytes = writeByte (rawVarFlags2 progress) }
          , ByteEdit { editOffset = g1VarFlags3 offsets,     editBytes = writeByte (rawVarFlags3 progress) }
          , ByteEdit { editOffset = g1VarFlags4 offsets,     editBytes = writeByte (rawVarFlags4 progress) }
          , ByteEdit { editOffset = g1VarFlags5 offsets,     editBytes = writeByte (rawVarFlags5 progress) }
          , ByteEdit { editOffset = g1VarFlags6 offsets,     editBytes = writeByte (rawVarFlags6 progress) }
          , ByteEdit { editOffset = g1VarFlags7 offsets,     editBytes = writeByte (rawVarFlags7 progress) }
          , ByteEdit { editOffset = g1VarFlags8 offsets,     editBytes = writeByte (rawVarFlags8 progress) }
          , ByteEdit { editOffset = g1DefeatedLorelei offsets, editBytes = rawDefeatedLorelei progress }
          , ByteEdit { editOffset = g1InGameTrades offsets,  editBytes = rawInGameTrades progress }
          , ByteEdit { editOffset = g1HiddenItems offsets,   editBytes = rawHiddenItems progress }
          , ByteEdit { editOffset = g1HiddenCoins offsets,   editBytes = rawHiddenCoins progress }
          , ByteEdit { editOffset = g1CurrentMap offsets,    editBytes = writeByte (rawCurrentMap progress) }
          -- Player position
          , ByteEdit { editOffset = g1PlayerY offsets,       editBytes = writeByte (rawPlayerY position) }
          , ByteEdit { editOffset = g1PlayerX offsets,       editBytes = writeByte (rawPlayerX position) }
          , ByteEdit { editOffset = g1LastMap offsets,       editBytes = writeByte (rawLastMap position) }
          , ByteEdit { editOffset = g1LastBlackoutMap offsets, editBytes = writeByte (rawLastBlackoutMap position) }
          , ByteEdit { editOffset = g1DestinationMap offsets, editBytes = writeByte (rawDestinationMap position) }
          -- Safari state
          , ByteEdit { editOffset = g1SafariSteps offsets,   editBytes = writeWord16BE (rawSafariSteps safari) }
          , ByteEdit { editOffset = g1SafariBallCount offsets, editBytes = writeByte (rawSafariBallCount safari) }
          , ByteEdit { editOffset = g1SafariGameOver offsets, editBytes = writeByte (rawSafariGameOver safari) }
          -- Fossil state
          , ByteEdit { editOffset = g1FossilItem offsets,    editBytes = writeByte (rawFossilItemGiven fossil) }
          , ByteEdit { editOffset = g1FossilResult offsets,  editBytes = rawFossilResult fossil }
          -- Transient state
          , ByteEdit { editOffset = g1LetterDelay offsets,   editBytes = writeByte (rawLetterDelay transient) }
          , ByteEdit { editOffset = g1MusicId offsets,       editBytes = writeByte (rawMusicId transient) }
          , ByteEdit { editOffset = g1MusicBank offsets,     editBytes = writeByte (rawMusicBank transient) }
          , ByteEdit { editOffset = g1ContrastId offsets,    editBytes = writeByte (rawContrastId transient) }
          , ByteEdit { editOffset = g1EnemyTrainerClass offsets, editBytes = writeByte (rawEnemyTrainerClass transient) }
          , ByteEdit { editOffset = g1BoulderSpriteIndex offsets, editBytes = writeByte (rawBoulderSpriteIndex transient) }
          , ByteEdit { editOffset = g1DungeonWarpDest offsets, editBytes = writeByte (rawDungeonWarpDest transient) }
          , ByteEdit { editOffset = g1DungeonWarpUsed offsets, editBytes = writeByte (rawDungeonWarpUsed transient) }
          , ByteEdit { editOffset = g1WarpedFromWarp offsets, editBytes = writeByte (rawWarpedFromWarp transient) }
          , ByteEdit { editOffset = g1WarpedFromMap offsets,  editBytes = writeByte (rawWarpedFromMap transient) }
          , ByteEdit { editOffset = g1CardKeyDoorY offsets,  editBytes = writeByte (rawCardKeyDoorY transient) }
          , ByteEdit { editOffset = g1CardKeyDoorX offsets,  editBytes = writeByte (rawCardKeyDoorX transient) }
          , ByteEdit { editOffset = g1TrashCanLock1 offsets, editBytes = writeByte (rawTrashCanLock1 transient) }
          , ByteEdit { editOffset = g1TrashCanLock2 offsets, editBytes = writeByte (rawTrashCanLock2 transient) }
          , ByteEdit { editOffset = g1CurrentMapScript offsets, editBytes = writeByte (rawCurrentMapScript transient) }
          ]
        patched     = applyPatches patches originalBytes
        checksum    = calculateGen1Checksum patched
                        (g1ChecksumStart offsets) (g1ChecksumEnd offsets)
        withChecksum = patchByte (g1Checksum offsets) checksum patched
    in patchBoxBanks nameLen boxCapacity
         (g1BoxBanks offsets) (rawGen1PCBoxes save) withChecksum


-- ── Container Serializers ────────────────────────────────────

serializeGen1Party :: NameLength -> ByteString -> RawGen1Party -> ByteString
serializeGen1Party nameLen original party =
  let nameLenInt   = unNameLength nameLen
      count        = fromIntegral (rawGen1PartyCount party) :: Int
      speciesStart = 1
      structsStart = 1 + gen1PartyCapacity + 1
      otStart      = structsStart + gen1PartyCapacity * gen1PartyPokemonSize
      nicksStart   = otStart + gen1PartyCapacity * nameLenInt
  in patchSlots (SaveOffset nicksStart) nameLenInt (rawGen1PartyNicknames party)
   $ patchSlots (SaveOffset otStart) nameLenInt (rawGen1PartyOTNames party)
   $ patchSlots (SaveOffset structsStart) gen1PartyPokemonSize
       (map serializeGen1PartyPokemon (rawGen1PartyMembers party))
   $ patchByte (SaveOffset (speciesStart + count)) 0xFF
   $ patchSlots (SaveOffset speciesStart) 1
       (map (ByteString.singleton . unInternalIndex) (rawGen1PartySpecies party))
   $ patchByte (SaveOffset 0) (rawGen1PartyCount party)
     original

serializeGen1Box :: NameLength -> BoxCapacity -> ByteString -> RawGen1Box -> ByteString
serializeGen1Box nameLen boxCapacity original box =
  let nameLenInt   = unNameLength nameLen
      boxCapInt    = unBoxCapacity boxCapacity
      count        = fromIntegral (rawGen1BoxCount box) :: Int
      speciesStart = 1
      structsStart = 1 + boxCapInt + 1
      otStart      = structsStart + boxCapInt * gen1BoxPokemonSize
      nicksStart   = otStart + boxCapInt * nameLenInt
  in patchSlots (SaveOffset nicksStart) nameLenInt (rawGen1BoxNicknames box)
   $ patchSlots (SaveOffset otStart) nameLenInt (rawGen1BoxOTNames box)
   $ patchSlots (SaveOffset structsStart) gen1BoxPokemonSize
       (map serializeGen1BoxPokemon (rawGen1BoxMembers box))
   $ patchByte (SaveOffset (speciesStart + count)) 0xFF
   $ patchSlots (SaveOffset speciesStart) 1
       (map (ByteString.singleton . unInternalIndex) (rawGen1BoxSpecies box))
   $ patchByte (SaveOffset 0) (rawGen1BoxCount box)
     original


-- ── Struct Serializers ───────────────────────────────────────

serializeGen1PartyPokemon :: RawGen1PartyPokemon -> ByteString
serializeGen1PartyPokemon pokemon =
  serializeGen1BoxPokemon (rawG1PartyBase pokemon)
  <> writeByte (rawG1Level pokemon)
  <> writeWord16BE (rawG1MaxHP pokemon)
  <> writeWord16BE (rawG1Attack pokemon)
  <> writeWord16BE (rawG1Defense pokemon)
  <> writeWord16BE (rawG1Speed pokemon)
  <> writeWord16BE (rawG1Special pokemon)

serializeGen1BoxPokemon :: RawGen1BoxPokemon -> ByteString
serializeGen1BoxPokemon pokemon =
  writeByte (unInternalIndex (rawG1BoxSpeciesIndex pokemon))
  <> writeWord16BE (rawG1BoxCurrentHP pokemon)
  <> writeByte (rawG1BoxBoxLevel pokemon)
  <> writeByte (rawG1BoxStatus pokemon)
  <> writeByte (rawG1BoxType1 pokemon)
  <> writeByte (rawG1BoxType2 pokemon)
  <> writeByte (rawG1BoxCatchRate pokemon)
  <> writeByte (rawG1BoxMove1 pokemon)
  <> writeByte (rawG1BoxMove2 pokemon)
  <> writeByte (rawG1BoxMove3 pokemon)
  <> writeByte (rawG1BoxMove4 pokemon)
  <> writeWord16BE (rawG1BoxOTID pokemon)
  <> writeWord24BE (rawG1BoxExp pokemon)
  <> serializeRawStatExp (rawG1BoxStatExp pokemon)
  <> writeWord16BE (rawG1BoxDVBytes pokemon)
  <> writeByte (rawG1BoxPP1 pokemon)
  <> writeByte (rawG1BoxPP2 pokemon)
  <> writeByte (rawG1BoxPP3 pokemon)
  <> writeByte (rawG1BoxPP4 pokemon)


-- ── Internal Helpers ─────────────────────────────────────────

serializeRawStatExp :: RawStatExp -> ByteString
serializeRawStatExp statExp =
  writeWord16BE (rawExpHP statExp)
  <> writeWord16BE (rawExpAttack statExp)
  <> writeWord16BE (rawExpDefense statExp)
  <> writeWord16BE (rawExpSpeed statExp)
  <> writeWord16BE (rawExpSpecial statExp)

serializeItemList :: [RawItemEntry] -> ByteString
serializeItemList items =
  let count = fromIntegral (length items) :: Word8
      entryBytes = concatMap (\entry -> [rawItemId entry, rawItemQuantity entry]) items
  in ByteString.pack (count : entryBytes ++ [0xFF])

serializeRawPlayTime :: RawPlayTime -> ByteString
serializeRawPlayTime playTime = ByteString.pack
  [ rawPlayHours playTime
  , rawPlayMaxed playTime
  , rawPlayMinutes playTime
  , rawPlaySeconds playTime
  , rawPlayFrames playTime
  ]


-- ── Hall of Fame Serialization ──────────────────────────────────

serializeHallOfFame :: NameLength -> ByteString -> [RawGen1HoFRecord] -> ByteString
serializeHallOfFame nameLen original records =
  foldl' patchRecord original (zip [0 ..] records)
  where
    patchRecord region (recordIndex, record) =
      foldl' (patchEntry recordIndex) region
        (zip [0 ..] (rawGen1HoFEntries record))

    patchEntry recordIndex region (entryIndex, entry) =
      let nameLenInt = unNameLength nameLen
          offset = recordIndex * gen1HoFRecordSize + entryIndex * gen1HoFEntrySize
      in patchByte (SaveOffset offset) (unInternalIndex (rawGen1HoFSpecies entry))
       $ patchByte (SaveOffset (offset + 1)) (rawGen1HoFLevel entry)
       $ patchBytes (SaveOffset (offset + 2)) (rawGen1HoFNickname entry)
       $ patchBytes (SaveOffset (offset + 2 + nameLenInt)) (rawGen1HoFPadding entry)
         region


-- ── Box Bank Serialization ────────────────────────────────────

patchBoxBanks :: NameLength -> BoxCapacity -> [BoxBankInfo] -> [RawGen1Box] -> ByteString -> ByteString
patchBoxBanks _ _ [] _ bytes = bytes
patchBoxBanks nameLen boxCapacity (bank : remainingBanks) allBoxes bytes =
  let bankCount = bankBoxCount bank
      (bankBoxes, remainingBoxes) = splitAt bankCount allBoxes
      withBoxData = foldl' (patchBoxData nameLen boxCapacity bank) bytes (zip [0..] bankBoxes)
      withBoxChecksums = foldl' (patchPerBoxChecksum bank) withBoxData
                           [0 .. bankCount - 1]
      bankChecksum = calculateGen1Checksum withBoxChecksums
                       (bankStartOffset bank)
                       (SaveOffset (unSaveOffset (bankAllChecksum bank) - 1))
  in patchBoxBanks nameLen boxCapacity remainingBanks remainingBoxes
       (patchByte (bankAllChecksum bank) bankChecksum withBoxChecksums)

patchBoxData :: NameLength -> BoxCapacity -> BoxBankInfo -> ByteString -> (Int, RawGen1Box) -> ByteString
patchBoxData nameLen boxCapacity bank current (boxIndex, box) =
  let boxOffset = unSaveOffset (bankStartOffset bank) + boxIndex * bankBoxDataSize bank
      originalRegion = ByteString.take (bankBoxDataSize bank)
                         (ByteString.drop boxOffset current)
  in patchBytes (SaveOffset boxOffset) (serializeGen1Box nameLen boxCapacity originalRegion box) current

patchPerBoxChecksum :: BoxBankInfo -> ByteString -> Int -> ByteString
patchPerBoxChecksum bank current boxIndex =
  let boxOffset = unSaveOffset (bankStartOffset bank) + boxIndex * bankBoxDataSize bank
      checksumValue = calculateGen1Checksum current
                        (SaveOffset boxOffset) (SaveOffset (boxOffset + bankBoxDataSize bank - 1))
  in patchByte (SaveOffset (unSaveOffset (bankBoxChecksums bank) + boxIndex)) checksumValue current

