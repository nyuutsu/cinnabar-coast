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

import Cinnabar.Binary (writeByte, writeWord16BE, writeWord24BE, patchByte, patchBytes, patchSlots)
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

        patched     = patchBytes (g1HallOfFame offsets)
                        (serializeHallOfFame nameLen originalHoFRegion
                          (rawGen1HallOfFame save))
                    $ patchBytes (g1PlayerName offsets) (rawGen1PlayerName save)
                    $ patchBytes (g1RivalName offsets)  (rawGen1RivalName save)
                    $ patchBytes (g1PartyData offsets)
                        (serializeGen1Party nameLen originalPartyRegion
                          (rawGen1Party save))
                    $ patchBytes (g1CurrentBox offsets)
                        (serializeGen1Box nameLen boxCapacity originalBoxRegion
                          (rawGen1CurrentBox save))
                    $ patchBytes (g1PokedexOwned offsets) (rawGen1PokedexOwned save)
                    $ patchBytes (g1PokedexSeen offsets)  (rawGen1PokedexSeen save)
                    $ patchBytes (g1BagItems offsets)
                        (serializeItemList (rawGen1BagItems save))
                    $ patchBytes (g1BoxItems offsets)
                        (serializeItemList (rawGen1BoxItems save))
                    $ patchBytes (g1Money offsets)        (rawGen1Money save)
                    $ patchBytes (g1CasinoCoins offsets)  (rawGen1CasinoCoins save)
                    $ patchByte  (g1Badges offsets)       (rawGen1Badges save)
                    $ patchBytes (g1PlayerID offsets)
                        (writeWord16BE (rawGen1PlayerID save))
                    $ patchByte  (g1Options offsets)      (rawGen1Options save)
                    $ patchByte  (g1CurrentBoxNumber offsets) (rawGen1CurrentBoxNum save)
                    $ patchByte  (g1HoFCount offsets)     (rawGen1HoFCount save)
                    $ patchBytes (g1PlayTime offsets)
                        (serializeRawPlayTime (rawGen1PlayTime save))
                    $ patchByte  (g1PikachuHappiness offsets) (rawGen1PikachuHappiness save)
                    $ patchByte  (g1PikachuMood offsets) (rawGen1PikachuMood save)
                    $ patchBytes (g1SurfingHiScore offsets) (rawGen1SurfingHiScore save)
                    $ patchByte  (g1PrinterSettings offsets) (rawGen1PrinterSettings save)
                    $ patchByte  (g1DaycareInUse offsets)
                        (rawDaycareInUse daycare)
                    $ patchByte  (g1DaycarePokemon offsets)
                        (unInternalIndex (rawDaycarePokemon daycare))
                    $ patchBytes (g1DaycareNickname offsets)
                        (rawDaycareNickname daycare)
                    $ patchBytes (g1DaycareOTName offsets)
                        (rawDaycareOTName daycare)
                    $ patchBytes (g1EventFlags offsets)
                        (rawEventFlags progress)
                    $ patchBytes (g1ToggleFlags offsets)
                        (rawToggleFlags progress)
                    $ patchBytes (g1MapScripts offsets)
                        (rawMapScripts progress)
                    $ patchByte  (g1DefeatedGyms offsets)
                        (rawDefeatedGyms progress)
                    $ patchByte  (g1PlayerStarter offsets)
                        (unInternalIndex (rawPlayerStarter progress))
                    $ patchByte  (g1RivalStarter offsets)
                        (unInternalIndex (rawRivalStarter progress))
                    $ patchBytes (g1TownsVisited offsets)
                        (rawTownsVisited progress)
                    $ patchByte  (g1MovementStatus offsets)
                        (rawMovementStatus progress)
                    $ patchByte  (g1VarFlags1 offsets) (rawVarFlags1 progress)
                    $ patchByte  (g1VarFlags2 offsets) (rawVarFlags2 progress)
                    $ patchByte  (g1VarFlags3 offsets) (rawVarFlags3 progress)
                    $ patchByte  (g1VarFlags4 offsets) (rawVarFlags4 progress)
                    $ patchByte  (g1VarFlags5 offsets) (rawVarFlags5 progress)
                    $ patchByte  (g1VarFlags6 offsets) (rawVarFlags6 progress)
                    $ patchByte  (g1VarFlags7 offsets) (rawVarFlags7 progress)
                    $ patchByte  (g1VarFlags8 offsets) (rawVarFlags8 progress)
                    $ patchBytes (g1DefeatedLorelei offsets)
                        (rawDefeatedLorelei progress)
                    $ patchBytes (g1InGameTrades offsets)
                        (rawInGameTrades progress)
                    $ patchBytes (g1HiddenItems offsets)
                        (rawHiddenItems progress)
                    $ patchBytes (g1HiddenCoins offsets)
                        (rawHiddenCoins progress)
                    $ patchByte  (g1CurrentMap offsets)
                        (rawCurrentMap progress)
                    -- Player position
                    $ patchByte  (g1PlayerY offsets) (rawPlayerY position)
                    $ patchByte  (g1PlayerX offsets) (rawPlayerX position)
                    $ patchByte  (g1LastMap offsets) (rawLastMap position)
                    $ patchByte  (g1LastBlackoutMap offsets) (rawLastBlackoutMap position)
                    $ patchByte  (g1DestinationMap offsets) (rawDestinationMap position)
                    -- Safari state
                    $ patchBytes (g1SafariSteps offsets)
                        (writeWord16BE (rawSafariSteps safari))
                    $ patchByte  (g1SafariBallCount offsets) (rawSafariBallCount safari)
                    $ patchByte  (g1SafariGameOver offsets) (rawSafariGameOver safari)
                    -- Fossil state
                    $ patchByte  (g1FossilItem offsets) (rawFossilItemGiven fossil)
                    $ patchBytes (g1FossilResult offsets) (rawFossilResult fossil)
                    -- Transient state
                    $ patchByte  (g1LetterDelay offsets) (rawLetterDelay transient)
                    $ patchByte  (g1MusicId offsets) (rawMusicId transient)
                    $ patchByte  (g1MusicBank offsets) (rawMusicBank transient)
                    $ patchByte  (g1ContrastId offsets) (rawContrastId transient)
                    $ patchByte  (g1EnemyTrainerClass offsets) (rawEnemyTrainerClass transient)
                    $ patchByte  (g1BoulderSpriteIndex offsets) (rawBoulderSpriteIndex transient)
                    $ patchByte  (g1DungeonWarpDest offsets) (rawDungeonWarpDest transient)
                    $ patchByte  (g1DungeonWarpUsed offsets) (rawDungeonWarpUsed transient)
                    $ patchByte  (g1WarpedFromWarp offsets) (rawWarpedFromWarp transient)
                    $ patchByte  (g1WarpedFromMap offsets) (rawWarpedFromMap transient)
                    $ patchByte  (g1CardKeyDoorY offsets) (rawCardKeyDoorY transient)
                    $ patchByte  (g1CardKeyDoorX offsets) (rawCardKeyDoorX transient)
                    $ patchByte  (g1TrashCanLock1 offsets) (rawTrashCanLock1 transient)
                    $ patchByte  (g1TrashCanLock2 offsets) (rawTrashCanLock2 transient)
                    $ patchByte  (g1CurrentMapScript offsets) (rawCurrentMapScript transient)
                    $ originalBytes
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
  serializeBoxFields pokemon
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

-- | The first 33 bytes of a party struct are the embedded box struct.
-- Shares field order with parseBoxFields.
serializeBoxFields :: RawGen1PartyPokemon -> ByteString
serializeBoxFields pokemon =
  serializeGen1BoxPokemon (rawG1PartyBase pokemon)

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

