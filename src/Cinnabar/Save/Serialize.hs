-- | Write path for Gen 1 save files.
--
-- Round-trip by construction: starts from the original ByteString,
-- patches only the fields that changed, recalculates the checksum.
-- Bytes we don't understand are preserved automatically.

module Cinnabar.Save.Serialize
  ( -- * Top-level serializer
    serializeGen1Save

    -- * Struct serializers
  , serializeGen1PartyMon
  , serializeGen1BoxMon
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word8)

import Cinnabar.Binary (writeByte, writeWord16BE, writeWord24BE, patchByte, patchBytes, patchSlots)
import Cinnabar.Save.Checksum (calculateGen1Checksum)
import Cinnabar.Save.Gen1.Raw (RawGen1PartyMon (..), RawGen1BoxMon (..), RawStatExp (..))
import Cinnabar.Save.Layout
  ( CartridgeLayout (..), SaveOffsets (..), Gen1SaveOffsets (..)
  , gen1PartyCapacity, gen1PartyMonSize, gen1BoxMonSize
  )
import Cinnabar.Save.Raw
  ( RawGen1SaveFile (..), RawGen1Party (..), RawGen1Box (..)
  , RawPlayTime (..), RawDaycare (..)
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

        partyRegionSize = 1 + (gen1PartyCapacity + 1)
                        + gen1PartyCapacity * gen1PartyMonSize
                        + gen1PartyCapacity * nameLen * 2
        originalPartyRegion = ByteString.take partyRegionSize
                            $ ByteString.drop (g1PartyData offsets) originalBytes

        boxRegionSize = 1 + (boxCapacity + 1)
                      + boxCapacity * gen1BoxMonSize
                      + boxCapacity * nameLen * 2
        originalBoxRegion = ByteString.take boxRegionSize
                          $ ByteString.drop (g1CurrentBox offsets) originalBytes

        patched     = patchBytes (g1PlayerName offsets) (rawGen1PlayerName save)
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
                    $ patchByte  (g1PikachuFriendship offsets) (rawGen1PikachuFriend save)
                    $ patchByte  (g1DaycareInUse offsets)
                        (rawDaycareInUse (rawGen1Daycare save))
                    $ patchByte  (g1DaycareMon offsets)
                        (unInternalIndex (rawDaycareMon (rawGen1Daycare save)))
                    $ originalBytes
        checksum    = calculateGen1Checksum patched
                        (g1ChecksumStart offsets) (g1ChecksumEnd offsets)
    in patchByte (g1Checksum offsets) checksum patched


-- ── Container Serializers ────────────────────────────────────

serializeGen1Party :: Int -> ByteString -> RawGen1Party -> ByteString
serializeGen1Party nameLen original party =
  let count        = fromIntegral (rawGen1PartyCount party) :: Int
      speciesStart = 1
      structsStart = 1 + gen1PartyCapacity + 1
      otStart      = structsStart + gen1PartyCapacity * gen1PartyMonSize
      nicksStart   = otStart + gen1PartyCapacity * nameLen
  in patchSlots nicksStart nameLen (rawGen1PartyNicks party)
   $ patchSlots otStart nameLen (rawGen1PartyOTNames party)
   $ patchSlots structsStart gen1PartyMonSize
       (map serializeGen1PartyMon (rawGen1PartyMons party))
   $ patchByte (speciesStart + count) 0xFF
   $ patchSlots speciesStart 1
       (map (ByteString.singleton . unInternalIndex) (rawGen1PartySpecies party))
   $ patchByte 0 (rawGen1PartyCount party)
     original

serializeGen1Box :: Int -> Int -> ByteString -> RawGen1Box -> ByteString
serializeGen1Box nameLen boxCapacity original box =
  let count        = fromIntegral (rawGen1BoxCount box) :: Int
      speciesStart = 1
      structsStart = 1 + boxCapacity + 1
      otStart      = structsStart + boxCapacity * gen1BoxMonSize
      nicksStart   = otStart + boxCapacity * nameLen
  in patchSlots nicksStart nameLen (rawGen1BoxNicks box)
   $ patchSlots otStart nameLen (rawGen1BoxOTNames box)
   $ patchSlots structsStart gen1BoxMonSize
       (map serializeGen1BoxMon (rawGen1BoxMons box))
   $ patchByte (speciesStart + count) 0xFF
   $ patchSlots speciesStart 1
       (map (ByteString.singleton . unInternalIndex) (rawGen1BoxSpecies box))
   $ patchByte 0 (rawGen1BoxCount box)
     original


-- ── Struct Serializers ───────────────────────────────────────

serializeGen1PartyMon :: RawGen1PartyMon -> ByteString
serializeGen1PartyMon mon =
  serializeBoxFields mon
  <> writeByte (rawG1Level mon)
  <> writeWord16BE (rawG1MaxHP mon)
  <> writeWord16BE (rawG1Attack mon)
  <> writeWord16BE (rawG1Defense mon)
  <> writeWord16BE (rawG1Speed mon)
  <> writeWord16BE (rawG1Special mon)

serializeGen1BoxMon :: RawGen1BoxMon -> ByteString
serializeGen1BoxMon mon =
  writeByte (unInternalIndex (rawG1BoxSpeciesIndex mon))
  <> writeWord16BE (rawG1BoxCurrentHP mon)
  <> writeByte (rawG1BoxBoxLevel mon)
  <> writeByte (rawG1BoxStatus mon)
  <> writeByte (rawG1BoxType1 mon)
  <> writeByte (rawG1BoxType2 mon)
  <> writeByte (rawG1BoxCatchRate mon)
  <> writeByte (rawG1BoxMove1 mon)
  <> writeByte (rawG1BoxMove2 mon)
  <> writeByte (rawG1BoxMove3 mon)
  <> writeByte (rawG1BoxMove4 mon)
  <> writeWord16BE (rawG1BoxOTID mon)
  <> writeWord24BE (rawG1BoxExp mon)
  <> serializeRawStatExp (rawG1BoxStatExp mon)
  <> writeWord16BE (rawG1BoxDVBytes mon)
  <> writeByte (rawG1BoxPP1 mon)
  <> writeByte (rawG1BoxPP2 mon)
  <> writeByte (rawG1BoxPP3 mon)
  <> writeByte (rawG1BoxPP4 mon)


-- ── Internal Helpers ─────────────────────────────────────────

-- | The first 33 bytes of a party struct are identical to a box struct.
-- Shares field order with parseBoxFields.
serializeBoxFields :: RawGen1PartyMon -> ByteString
serializeBoxFields mon =
  writeByte (unInternalIndex (rawG1SpeciesIndex mon))
  <> writeWord16BE (rawG1CurrentHP mon)
  <> writeByte (rawG1BoxLevel mon)
  <> writeByte (rawG1Status mon)
  <> writeByte (rawG1Type1 mon)
  <> writeByte (rawG1Type2 mon)
  <> writeByte (rawG1CatchRate mon)
  <> writeByte (rawG1Move1 mon)
  <> writeByte (rawG1Move2 mon)
  <> writeByte (rawG1Move3 mon)
  <> writeByte (rawG1Move4 mon)
  <> writeWord16BE (rawG1OTID mon)
  <> writeWord24BE (rawG1Exp mon)
  <> serializeRawStatExp (rawG1StatExp mon)
  <> writeWord16BE (rawG1DVBytes mon)
  <> writeByte (rawG1PP1 mon)
  <> writeByte (rawG1PP2 mon)
  <> writeByte (rawG1PP3 mon)
  <> writeByte (rawG1PP4 mon)

serializeRawStatExp :: RawStatExp -> ByteString
serializeRawStatExp statExp =
  writeWord16BE (rawExpHP statExp)
  <> writeWord16BE (rawExpAttack statExp)
  <> writeWord16BE (rawExpDefense statExp)
  <> writeWord16BE (rawExpSpeed statExp)
  <> writeWord16BE (rawExpSpecial statExp)

serializeItemList :: [(Word8, Word8)] -> ByteString
serializeItemList items =
  let count = fromIntegral (length items) :: Word8
      entryBytes = concatMap (\(itemId, quantity) -> [itemId, quantity]) items
  in ByteString.pack (count : entryBytes ++ [0xFF])

serializeRawPlayTime :: RawPlayTime -> ByteString
serializeRawPlayTime playTime = ByteString.pack
  [ rawPlayHours playTime
  , rawPlayMaxed playTime
  , rawPlayMinutes playTime
  , rawPlaySeconds playTime
  , rawPlayFrames playTime
  ]

