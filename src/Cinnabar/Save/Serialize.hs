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

import Cinnabar.Binary (writeByte, writeWord16BE, writeWord24BE, patchBytes, patchByte)
import Cinnabar.Save.Checksum (calculateGen1Checksum)
import Cinnabar.Save.Gen1.Raw (RawGen1PartyMon (..), RawGen1BoxMon (..), RawStatExp (..))
import Cinnabar.Save.Layout
  ( CartridgeLayout (..), SaveOffsets (..), Gen1SaveOffsets (..) )
import Cinnabar.Save.Raw (RawGen1SaveFile (..), RawGen1Party (..), RawGen1Box (..))
import Cinnabar.Types (InternalIndex (..))


-- ── Top-Level Serializer ─────────────────────────────────────

serializeGen1Save :: RawGen1SaveFile -> ByteString
serializeGen1Save save = case layoutOffsets (rawGen1Layout save) of
  Gen2Offsets _ -> error "serializeGen1Save: Gen 2 offsets on a Gen 1 save"
  Gen1Offsets offsets ->
    let nameLen     = layoutNameLen (rawGen1Layout save)
        boxCapacity = layoutBoxCapacity (rawGen1Layout save)
        patched     = patchBytes (g1PlayerName offsets) (rawGen1PlayerName save)
                    $ patchBytes (g1RivalName offsets)  (rawGen1RivalName save)
                    $ patchBytes (g1PartyData offsets)
                        (serializeGen1Party nameLen (rawGen1Party save))
                    $ patchBytes (g1CurrentBox offsets)
                        (serializeGen1Box nameLen boxCapacity (rawGen1CurrentBox save))
                    $ rawGen1Bytes save
        checksum    = calculateGen1Checksum patched
                        (g1ChecksumStart offsets) (g1ChecksumEnd offsets)
    in patchByte (g1Checksum offsets) checksum patched


-- ── Container Serializers ────────────────────────────────────

gen1PartyCapacity :: Int
gen1PartyCapacity = 6

gen1PartyMonSize :: Int
gen1PartyMonSize = 44

gen1BoxMonSize :: Int
gen1BoxMonSize = 33

serializeGen1Party :: Int -> RawGen1Party -> ByteString
serializeGen1Party nameLen party =
  writeByte (rawGen1PartyCount party)
  <> serializeSpeciesList gen1PartyCapacity (rawGen1PartySpecies party)
  <> serializeFixedSlots gen1PartyCapacity
       gen1PartyMonSize serializeGen1PartyMon (rawGen1PartyMons party)
  <> serializeNameSlots gen1PartyCapacity
       nameLen (rawGen1PartyOTNames party)
  <> serializeNameSlots gen1PartyCapacity
       nameLen (rawGen1PartyNicks party)

serializeGen1Box :: Int -> Int -> RawGen1Box -> ByteString
serializeGen1Box nameLen boxCapacity box =
  writeByte (rawGen1BoxCount box)
  <> serializeSpeciesList boxCapacity (rawGen1BoxSpecies box)
  <> serializeFixedSlots boxCapacity
       gen1BoxMonSize serializeGen1BoxMon (rawGen1BoxMons box)
  <> serializeNameSlots boxCapacity
       nameLen (rawGen1BoxOTNames box)
  <> serializeNameSlots boxCapacity
       nameLen (rawGen1BoxNicks box)


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

-- | Species list: entries + 0xFF terminator + zero padding to
-- fill (capacity + 1) bytes total.
serializeSpeciesList :: Int -> [InternalIndex] -> ByteString
serializeSpeciesList capacity species =
  let totalSize   = capacity + 1
      entryBytes  = ByteString.pack (map unInternalIndex species)
      terminator  = ByteString.singleton 0xFF
      paddingSize = totalSize - ByteString.length entryBytes - 1
  in entryBytes <> terminator <> ByteString.replicate paddingSize 0x00

-- | Serialize populated entries, then zero-fill unused slots.
serializeFixedSlots :: Int -> Int -> (a -> ByteString) -> [a] -> ByteString
serializeFixedSlots capacity slotSize serializer entries =
  let populated = mconcat (map serializer entries)
      unused    = capacity - length entries
  in populated <> ByteString.replicate (unused * slotSize) 0x00

-- | Serialize name bytestrings for populated slots, then zero-fill unused.
serializeNameSlots :: Int -> Int -> [ByteString] -> ByteString
serializeNameSlots capacity nameLen names =
  let populated = mconcat (map (padName nameLen) names)
      unused    = capacity - length names
  in populated <> ByteString.replicate (unused * nameLen) 0x00

-- | Ensure a name is exactly nameLen bytes, padding with zeroes
-- if shorter (shouldn't happen with well-formed data, but safe).
padName :: Int -> ByteString -> ByteString
padName nameLen nameBytes
  | ByteString.length nameBytes >= nameLen = ByteString.take nameLen nameBytes
  | otherwise = nameBytes <> ByteString.replicate (nameLen - ByteString.length nameBytes) 0x00
