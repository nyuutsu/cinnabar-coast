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

import Cinnabar.Binary (writeByte, writeWord16BE, writeWord24BE, patchByte, patchBytes, patchSlots, applyPatches)
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
                            $ ByteString.drop (g1PartyData offsets) originalBytes

        boxRegionSize = 1 + (unBoxCapacity boxCapacity + 1)
                      + unBoxCapacity boxCapacity * gen1BoxPokemonSize
                      + unBoxCapacity boxCapacity * unNameLength nameLen * 2
        originalBoxRegion = ByteString.take boxRegionSize
                          $ ByteString.drop (g1CurrentBox offsets) originalBytes

        hofRegionSize = gen1HoFRecordCount * gen1HoFRecordSize
        originalHoFRegion = ByteString.take hofRegionSize
                          $ ByteString.drop (g1HallOfFame offsets) originalBytes

        patches =
          -- Main data regions
          [ (g1HallOfFame offsets,
              serializeHallOfFame nameLen originalHoFRegion (rawGen1HallOfFame save))
          , (g1PlayerName offsets,    rawGen1PlayerName save)
          , (g1RivalName offsets,     rawGen1RivalName save)
          , (g1PartyData offsets,
              serializeGen1Party nameLen originalPartyRegion (rawGen1Party save))
          , (g1CurrentBox offsets,
              serializeGen1Box nameLen boxCapacity originalBoxRegion
                (rawGen1CurrentBox save))
          , (g1PokedexOwned offsets,  rawGen1PokedexOwned save)
          , (g1PokedexSeen offsets,   rawGen1PokedexSeen save)
          , (g1BagItems offsets,      serializeItemList (rawGen1BagItems save))
          , (g1BoxItems offsets,      serializeItemList (rawGen1BoxItems save))
          , (g1Money offsets,         rawGen1Money save)
          , (g1CasinoCoins offsets,   rawGen1CasinoCoins save)
          , (g1Badges offsets,        writeByte (rawGen1Badges save))
          , (g1PlayerID offsets,      writeWord16BE (rawGen1PlayerID save))
          , (g1Options offsets,       writeByte (rawGen1Options save))
          , (g1CurrentBoxNumber offsets, writeByte (rawGen1CurrentBoxNum save))
          , (g1HoFCount offsets,      writeByte (rawGen1HoFCount save))
          , (g1PlayTime offsets,      serializeRawPlayTime (rawGen1PlayTime save))
          , (g1PikachuHappiness offsets, writeByte (rawGen1PikachuHappiness save))
          , (g1PikachuMood offsets,   writeByte (rawGen1PikachuMood save))
          , (g1SurfingHiScore offsets, rawGen1SurfingHiScore save)
          , (g1PrinterSettings offsets, writeByte (rawGen1PrinterSettings save))
          -- Daycare
          , (g1DaycareInUse offsets,  writeByte (rawDaycareInUse daycare))
          , (g1DaycarePokemon offsets, writeByte (unInternalIndex (rawDaycarePokemon daycare)))
          , (g1DaycareNickname offsets, rawDaycareNickname daycare)
          , (g1DaycareOTName offsets, rawDaycareOTName daycare)
          -- Progress flags
          , (g1EventFlags offsets,    rawEventFlags progress)
          , (g1ToggleFlags offsets,   rawToggleFlags progress)
          , (g1MapScripts offsets,    rawMapScripts progress)
          , (g1DefeatedGyms offsets,  writeByte (rawDefeatedGyms progress))
          , (g1PlayerStarter offsets, writeByte (unInternalIndex (rawPlayerStarter progress)))
          , (g1RivalStarter offsets,  writeByte (unInternalIndex (rawRivalStarter progress)))
          , (g1TownsVisited offsets,  rawTownsVisited progress)
          , (g1MovementStatus offsets, writeByte (rawMovementStatus progress))
          , (g1VarFlags1 offsets,     writeByte (rawVarFlags1 progress))
          , (g1VarFlags2 offsets,     writeByte (rawVarFlags2 progress))
          , (g1VarFlags3 offsets,     writeByte (rawVarFlags3 progress))
          , (g1VarFlags4 offsets,     writeByte (rawVarFlags4 progress))
          , (g1VarFlags5 offsets,     writeByte (rawVarFlags5 progress))
          , (g1VarFlags6 offsets,     writeByte (rawVarFlags6 progress))
          , (g1VarFlags7 offsets,     writeByte (rawVarFlags7 progress))
          , (g1VarFlags8 offsets,     writeByte (rawVarFlags8 progress))
          , (g1DefeatedLorelei offsets, rawDefeatedLorelei progress)
          , (g1InGameTrades offsets,  rawInGameTrades progress)
          , (g1HiddenItems offsets,   rawHiddenItems progress)
          , (g1HiddenCoins offsets,   rawHiddenCoins progress)
          , (g1CurrentMap offsets,    writeByte (rawCurrentMap progress))
          -- Player position
          , (g1PlayerY offsets,       writeByte (rawPlayerY position))
          , (g1PlayerX offsets,       writeByte (rawPlayerX position))
          , (g1LastMap offsets,       writeByte (rawLastMap position))
          , (g1LastBlackoutMap offsets, writeByte (rawLastBlackoutMap position))
          , (g1DestinationMap offsets, writeByte (rawDestinationMap position))
          -- Safari state
          , (g1SafariSteps offsets,   writeWord16BE (rawSafariSteps safari))
          , (g1SafariBallCount offsets, writeByte (rawSafariBallCount safari))
          , (g1SafariGameOver offsets, writeByte (rawSafariGameOver safari))
          -- Fossil state
          , (g1FossilItem offsets,    writeByte (rawFossilItemGiven fossil))
          , (g1FossilResult offsets,  rawFossilResult fossil)
          -- Transient state
          , (g1LetterDelay offsets,   writeByte (rawLetterDelay transient))
          , (g1MusicId offsets,       writeByte (rawMusicId transient))
          , (g1MusicBank offsets,     writeByte (rawMusicBank transient))
          , (g1ContrastId offsets,    writeByte (rawContrastId transient))
          , (g1EnemyTrainerClass offsets, writeByte (rawEnemyTrainerClass transient))
          , (g1BoulderSpriteIndex offsets, writeByte (rawBoulderSpriteIndex transient))
          , (g1DungeonWarpDest offsets, writeByte (rawDungeonWarpDest transient))
          , (g1DungeonWarpUsed offsets, writeByte (rawDungeonWarpUsed transient))
          , (g1WarpedFromWarp offsets, writeByte (rawWarpedFromWarp transient))
          , (g1WarpedFromMap offsets,  writeByte (rawWarpedFromMap transient))
          , (g1CardKeyDoorY offsets,  writeByte (rawCardKeyDoorY transient))
          , (g1CardKeyDoorX offsets,  writeByte (rawCardKeyDoorX transient))
          , (g1TrashCanLock1 offsets, writeByte (rawTrashCanLock1 transient))
          , (g1TrashCanLock2 offsets, writeByte (rawTrashCanLock2 transient))
          , (g1CurrentMapScript offsets, writeByte (rawCurrentMapScript transient))
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
  in patchSlots nicksStart nameLenInt (rawGen1PartyNicknames party)
   $ patchSlots otStart nameLenInt (rawGen1PartyOTNames party)
   $ patchSlots structsStart gen1PartyPokemonSize
       (map serializeGen1PartyPokemon (rawGen1PartyMembers party))
   $ patchByte (speciesStart + count) 0xFF
   $ patchSlots speciesStart 1
       (map (ByteString.singleton . unInternalIndex) (rawGen1PartySpecies party))
   $ patchByte 0 (rawGen1PartyCount party)
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
  in patchSlots nicksStart nameLenInt (rawGen1BoxNicknames box)
   $ patchSlots otStart nameLenInt (rawGen1BoxOTNames box)
   $ patchSlots structsStart gen1BoxPokemonSize
       (map serializeGen1BoxPokemon (rawGen1BoxMembers box))
   $ patchByte (speciesStart + count) 0xFF
   $ patchSlots speciesStart 1
       (map (ByteString.singleton . unInternalIndex) (rawGen1BoxSpecies box))
   $ patchByte 0 (rawGen1BoxCount box)
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
      in patchByte offset (unInternalIndex (rawGen1HoFSpecies entry))
       $ patchByte (offset + 1) (rawGen1HoFLevel entry)
       $ patchBytes (offset + 2) (rawGen1HoFNickname entry)
       $ patchBytes (offset + 2 + nameLenInt) (rawGen1HoFPadding entry)
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
                       (bankStartOffset bank) (bankAllChecksum bank - 1)
  in patchBoxBanks nameLen boxCapacity remainingBanks remainingBoxes
       (patchByte (bankAllChecksum bank) bankChecksum withBoxChecksums)

patchBoxData :: NameLength -> BoxCapacity -> BoxBankInfo -> ByteString -> (Int, RawGen1Box) -> ByteString
patchBoxData nameLen boxCapacity bank current (boxIndex, box) =
  let boxOffset = bankStartOffset bank + boxIndex * bankBoxDataSize bank
      originalRegion = ByteString.take (bankBoxDataSize bank)
                         (ByteString.drop boxOffset current)
  in patchBytes boxOffset (serializeGen1Box nameLen boxCapacity originalRegion box) current

patchPerBoxChecksum :: BoxBankInfo -> ByteString -> Int -> ByteString
patchPerBoxChecksum bank current boxIndex =
  let boxOffset = bankStartOffset bank + boxIndex * bankBoxDataSize bank
      checksumValue = calculateGen1Checksum current
                        boxOffset (boxOffset + bankBoxDataSize bank - 1)
  in patchByte (bankBoxChecksums bank + boxIndex) checksumValue current

