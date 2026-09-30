# Generation I save map, read from the disassembly

Every byte of a Western Red/Blue/Yellow save, with the reason it is there.

`gen1-save-structure.md` is a transcription of Bulbapedia's article.
This document is derived from pret's `pokered` and `pokeyellow`, assembled with rgbds 1.0.4, and read from the linker's symbol files.
Both disassemblies build byte-identical copies of the retail ROMs.
Every WRAM address the code touches is therefore pinned by the ROM itself, so the offsets below are authoritative, not transcribed.
A save offset is `bank * 0x2000 + (address - 0xA000)`.

## The one idea: bank 1 is a memory snapshot

Saving the game is five `CopyData` calls from WRAM into SRAM bank 1 (`SaveMainData`, `SaveCurrentBoxData`, `SavePartyAndDexData` in `engine/menus/save.asm`), plus one HRAM byte:

| Save offset | Size | SRAM label | Copied from (Red) | Copied from (Yellow) |
|---|---|---|---|---|
| 0x2598 | 0x00B | `sPlayerName` | `wPlayerName` D158 | D157 |
| 0x25A3 | 0x789 | `sMainData` | `wMainDataStart`..`wMainDataEnd` D2F7 | D2F6 |
| 0x2D2C | 0x200 | `sSpriteData` | `wSpriteDataStart`..`wSpriteDataEnd` C100 | C100 |
| 0x2F2C | 0x194 | `sPartyData` | `wPartyDataStart`..`wPartyDataEnd` D163 | D162 |
| 0x30C0 | 0x462 | `sCurBoxData` | `wBoxDataStart`..`wBoxDataEnd` DA80 | DA7F |
| 0x3522 | 0x001 | `sTileAnimations` | `hTileAnimations` (HRAM) | same |
| 0x3523 | 0x001 | `sMainDataCheckSum` | computed | same |

Game Freak did not pick fields to save; they saved whole WRAM regions, and whatever the linker placed inside those regions came along.
That is why map-rendering pointers, the last opponent's party, and 150-odd bytes of `ds` padding sit inside a save file.

The SRAM offsets are identical across Red, Blue and Yellow, but the WRAM sources are not: Yellow's regions (except sprite data) sit one byte lower.
Bulbapedia's "Copied To" column (D2F7, difference 0xAD54) is Red's.
Converting a pokeyellow WRAM label with Red's difference lands one byte early.
Cinnabar's four Yellow Pikachu-block offsets carry exactly that error (see "Corrections" below).

## What happens on Continue

This is the fact that turns "scratch, or unwise to edit" into something precise.

`LoadMainData` copies the snapshot back into WRAM, then sets bit 7 of `wCurMapTileset` (`BIT_NO_PREVIOUS_MAP`).
Continue then runs `SpecialEnterMap` → `EnterMap` → `LoadMapData` → `LoadMapHeader`.
`LoadMapHeader` calls `MarkTownVisitedAndLoadToggleableObjects`, then sees bit 7, clears it, and **returns before loading anything from ROM** (`home/overworld.asm`, `LoadMapHeader`).
The map header, connections, warps, signs, NPC table, tileset header, wild encounter tables and map music are all skipped.
The game resumes on the copies in the save.
Those copies stay authoritative until the next map transition, when `LoadMapHeader` runs in full.

So the bytes Bulbapedia calls "reloaded from ROM" are not reloaded on Continue.
They are a cache of ROM data for the map in `wCurMap`, and the save only works if the cache and `wCurMap` agree.
Changing `wCurMap` alone gives you map N's ID with map M's header, warps and collision: the classic corrupted-teleport save.
Relocating the player means regenerating the whole cache block from ROM data for the new map, which `extract/` is in a position to supply.

Continue also forces a few values regardless of the save.
`wPlayerDirection` becomes down.
`wCableClubDestinationMap` (the same byte as `wStatusFlags3`) becomes 0.
`ResetPlayerSpriteData` rewrites sprite slot 0.
`LoadMapData` recomputes the VRAM view pointer and clears `wSpriteSetID`.
The toggleable-object list is rebuilt for the current map.
If the save was made in the Hall of Fame room with at least one win, the player is warped to Pallet Town.

## Categories

Each row below carries one category.
They replace the old "parsed / scratch / not parsed" split.

- **state**: game progress and possessions. The reason the save exists.
- **setting**: a player choice (options, text delay).
- **map cache**: copied from ROM when a map loads, reused as-is on Continue, so it must agree with `wCurMap`.
- **in flight**: state of something mid-action: a warp, a boulder push, a Safari trip, the last battle. Meaningful at the instant of saving.
- **rebuilt**: overwritten on Continue before anything reads it. Editing it has no effect.
- **union**: one span with two meanings depending on context.
- **unused**: a labeled byte the code writes and never reads.
- **gap**: an anonymous `ds N`. No label, no code reference.
- **scratch**: SRAM borrowed as work RAM.

## Bank 0 (0x0000–0x1FFF): scratch and the Hall of Fame

Nothing in bank 0 is checksummed.

| Offset | Size | Label | Category | Notes |
|---|---|---|---|---|
| 0x0000 | 0x188 | `sSpriteBuffer0` | scratch | `SPRITEBUFFERSIZE` = 7×7 tiles × 8 bytes: one 1bpp plane of the largest Pokémon pic. |
| 0x0188 | 0x188 | `sSpriteBuffer1` | scratch | Second plane. |
| 0x0310 | 0x188 | `sSpriteBuffer2` | scratch | Output of the delta/XOR merge. |
| 0x0498 | 0x100 | — | gap | `ds $100`. |
| 0x0598 | 0x12C0 | `sHallOfFame` | state | 50 teams × `HOF_TEAM` (6 × `HOF_MON` = 16 bytes: species, level, 11-byte nickname, 3 unused). |
| 0x1858 | 0x7A8 | — | gap | Past the last section; nothing addresses it. |

**Sprite buffers.** Pic decompression (`home/uncompress.asm`) needs three 392-byte planes.
The Game Boy has 8 KiB of WRAM and the overworld state fills most of it, so the decompressor works in cartridge RAM instead.
The HoF movie, Oak's speech, battle sprite scaling and Yellow's Pikachu animations use the same buffers.
Their contents on disk are whatever the last decompression left.
Yellow's title screen fills the first 0x20 bytes of buffer 0 with 0xAA; that is a weak Yellow fingerprint, not proof.

**Hall of Fame.** `wNumHoFTeams` (0x284E) counts wins and saturates at 255 instead of wrapping.
`SaveHallOfFameTeams` writes win *n* to slot *n−1* while *n* ≤ 50.
After that it shifts slots 1–49 down one and writes the new team to slot 49, so the array is always oldest-first.
The League PC labels the first stored team `count − 50 + 1`.

## Bank 1 (0x2000–0x3FFF): the snapshot

Checksum: `CalcCheckSum` over 0x2598–0x3522 inclusive (`sGameData`..`sGameDataEnd`): an 8-bit sum, complemented, stored at 0x3523.
It covers every gap and union in the range, so any edit inside it requires recomputing.

| Offset | Size | Label | Category |
|---|---|---|---|
| 0x2000 | 0x598 | — | gap (`ds $598` before `sGameData`) |
| 0x2598 | 0x00B | `sPlayerName` | state |
| 0x25A3 | 0x789 | Main Data | see below |
| 0x2D2C | 0x200 | Sprite Data | see below |
| 0x2F2C | 0x194 | Party Data | state |
| 0x30C0 | 0x462 | Current Box Data | state |
| 0x3522 | 0x001 | `sTileAnimations` | map cache |
| 0x3523 | 0x001 | `sMainDataCheckSum` | checksum |
| 0x3524 | 0xADC | — | gap |

0x598 equals the size of bank 0's three sprite buffers plus their `ds $100`, so `sGameData` in bank 1 lands at the same bank-relative address (A598) as `sHallOfFame` in bank 0.
Nothing in the code depends on that; the ASM just says `ds $598`.

### Main Data (0x25A3–0x2D2B)

"✓" means cinnabar's layout reads the field today.

| Offset | Size | Label | Category | Notes |
|---|---|---|---|---|
| 0x25A3 | 19 | `wPokedexOwned` | state ✓ | 152 bits; bit 151 exists but no species uses it. |
| 0x25B6 | 19 | `wPokedexSeen` | state ✓ | |
| 0x25C9 | 1+41 | `wNumBagItems`, `wBagItems` | state ✓ | 20 pairs + 0xFF. |
| 0x25F3 | 3 | `wPlayerMoney` | state ✓ | BCD, big-endian. |
| 0x25F6 | 11 | `wRivalName` | state ✓ | |
| 0x2601 | 1 | `wOptions` | setting ✓ | |
| 0x2602 | 1 | `wObtainedBadges` | state ✓ | |
| 0x2603 | 1 | `wUnusedObtainedBadges` | unused | |
| 0x2604 | 1 | `wLetterPrintingDelayFlags` | setting ✓ | Set by `InitOptions`; cinnabar files it as transient. |
| 0x2605 | 2 | `wPlayerID` | state ✓ | |
| 0x2607 | 1 | `wMapMusicSoundID` | map cache ✓ | Plays on Continue. Cinnabar: transient. |
| 0x2608 | 1 | `wMapMusicROMBank` | map cache ✓ | |
| 0x2609 | 1 | `wMapPalOffset` | in flight ✓ | 6 in an unlit Flash cave; stays dark after Continue. |
| 0x260A | 1 | `wCurMap` | state ✓ | The key every map-cache byte must agree with. |
| 0x260B | 2 | `wCurrentTileBlockMapViewPointer` | map cache | Must agree with the coordinates. |
| 0x260D | 1 | `wYCoord` | state ✓ | |
| 0x260E | 1 | `wXCoord` | state ✓ | |
| 0x260F | 1 | `wYBlockCoord` | map cache | Which half of the 2×2 block; derived from the coordinates. |
| 0x2610 | 1 | `wXBlockCoord` | map cache | |
| 0x2611 | 1 | `wLastMap` | state ✓ | Where "outside" is when leaving a building. |
| 0x2612 | 1 | `wUnusedLastMapWidth` | unused | |
| 0x2613 | 1 | `wCurMapTileset` | map cache | Bit 7 is the keep-this-header flag `LoadMainData` sets. |
| 0x2614 | 2 | `wCurMapHeight`, `wCurMapWidth` | map cache | In 4×4 blocks. |
| 0x2616 | 6 | `wCurMapDataPtr`, `TextPtr`, `ScriptPtr` | map cache | ROM pointers into the map's bank. |
| 0x261C | 1 | `wCurMapConnections` | map cache | Which of the four headers below are live. |
| 0x261D | 4×11 | `w{North,South,West,East}ConnectionHeader` | map cache | Map, strip source/dest/length, width, alignment, view pointer. |
| 0x2649 | 11 | `wSpriteSet` | map cache | |
| 0x2654 | 1 | `wSpriteSetID` | rebuilt | Cleared by `LoadMapData`, then recomputed. |
| 0x2655 | 2 | `wObjectDataPointerTemp` | map cache | |
| 0x2657 | 2 | — | gap | |
| 0x2659 | 1 | `wMapBackgroundTile` | map cache | |
| 0x265A | 1+128 | `wNumberOfWarps`, `wWarpEntries` | map cache | 32 × (Y, X, warp ID, map). |
| 0x26DB | 1 | `wDestinationWarpID` | in flight | |
| 0x26DC | 128 | Red/Blue: — / Yellow: Pikachu block | gap / see below | Red's `ds 128` is the space Yellow fills. |
| 0x275C | 1+32+16 | `wNumSigns`, `wSignCoords`, `wSignTextIDs` | map cache | |
| 0x278D | 1 | `wNumSprites` | map cache | |
| 0x278E | 2 | `w{Y,X}OffsetSinceLastSpecialWarp` | unused | pret: "they don't seem to be used for anything". |
| 0x2790 | 32 | `wMapSpriteData` | map cache | Per NPC: movement byte 2, text ID. |
| 0x27B0 | 32 | `wMapSpriteExtraData` | map cache | Per NPC: trainer class + set, or item ID. |
| 0x27D0 | 2 | `wCurrentMapHeight2`, `Width2` | map cache | Height/width doubled. |
| 0x27D2 | 2 | `wMapViewVRAMPointer` | rebuilt | `LoadMapData` resets it. |
| 0x27D4 | 1 | `wPlayerMovingDirection` | in flight | |
| 0x27D5 | 1 | `wPlayerLastStopDirection` | in flight | |
| 0x27D6 | 1 | `wPlayerDirection` | rebuilt | Continue forces "down". |
| 0x27D7 | 11 | `wTilesetBank`, `BlocksPtr`, `GfxPtr`, `CollisionPtr`, `TalkingOverTiles` | map cache | Tileset header. |
| 0x27E1 | 1 | `wGrassTile` | map cache | |
| 0x27E2 | 4 | — | gap | |
| 0x27E6 | 1+101 | `wNumBoxItems`, `wBoxItems` | state ✓ | 50 pairs + 0xFF. |
| 0x284C | 1 | `wCurrentBoxNum` | state ✓ | Bits 0–6: box. Bit 7: has changed boxes before (see bank 2). |
| 0x284D | 1 | — | gap | Not part of the box number. |
| 0x284E | 1 | `wNumHoFTeams` | state ✓ | Wins, saturating at 255. |
| 0x284F | 1 | `wUnusedMapVariable` | unused | Zeroed on every map entry. |
| 0x2850 | 2 | `wPlayerCoins` | state ✓ | BCD. |
| 0x2852 | 32 | `wToggleableObjectFlags` | state ✓ | Set = object hidden (item taken, Snorlax gone, …). |
| 0x2872 | 7 | — | gap | |
| 0x2879 | 1 | `wSavedSpriteImageIndex` | in flight | |
| 0x287A | 33 | `wToggleableObjectList` | rebuilt | Rebuilt for `wCurMap` before the early return. |
| 0x289B | 1 | — | gap | |
| 0x289C | 200 | `wGameProgressFlags` | state ✓ | One script index per map, with `ds` holes between entries. |
| 0x2964 | 56 | — | gap | Cinnabar's 256-byte read covers this too. |
| 0x299C | 14 | `wObtainedHiddenItemsFlags` | state ✓ | |
| 0x29AA | 2 | `wObtainedHiddenCoinsFlags` | state ✓ | |
| 0x29AC | 1 | `wWalkBikeSurfState` | state ✓ | |
| 0x29AD | 10 | — | gap | |
| 0x29B7 | 2 | `wTownVisitedFlag` | state ✓ | Fly destinations. |
| 0x29B9 | 2 | `wSafariSteps` | in flight ✓ | Big-endian by the code's own reads. |
| 0x29BB | 1 | `wFossilItem` | state ✓ | |
| 0x29BC | 1 | `wFossilMon` | state ✓ | One byte, then a 2-byte gap. |
| 0x29BD | 2 | — | gap | |
| 0x29BF | 1 | `wEnemyMonOrTrainerClass` | in flight ✓ | |
| 0x29C0 | 1 | `wPlayerJumpingYScreenCoordsIndex` | in flight | |
| 0x29C1 | 1 | `wRivalStarter` | state ✓ | Yellow routes the rival's Eevee evolution through this. |
| 0x29C2 | 1 | — | gap | |
| 0x29C3 | 1 | `wPlayerStarter` | state ✓ | |
| 0x29C4 | 1 | `wBoulderSpriteIndex` | in flight ✓ | |
| 0x29C5 | 1 | `wLastBlackoutMap` | state ✓ | |
| 0x29C6 | 1 | `wDestinationMap` | in flight ✓ | |
| 0x29C7 | 1 | `wUnusedPlayerDataByte` | unused | pret: "initialized to $ff, but nothing ever reads it". |
| 0x29C8 | 1 | `wTileInFrontOfBoulderAndBoulderCollisionResult` | in flight | |
| 0x29C9 | 1 | `wDungeonWarpDestinationMap` | in flight ✓ | |
| 0x29CA | 1 | `wWhichDungeonWarp` | in flight ✓ | |
| 0x29CB | 1 | `wUnusedCardKeyGateID` | unused | |
| 0x29CC | 8 | — | gap | |
| 0x29D4 | 1 | `wStatusFlags1` | mixed ✓ | Rods and the Saffron drink are state; Strength-active is in flight. |
| 0x29D5 | 1 | — | gap | |
| 0x29D6 | 1 | `wBeatGymFlags` | state ✓ | pret: "redundant because it matches wObtainedBadges". |
| 0x29D7 | 1 | — | gap | |
| 0x29D8 | 1 | `wStatusFlags2` | in flight ✓ | |
| 0x29D9 | 1 | `wStatusFlags3` = `wCableClubDestinationMap` | rebuilt ✓ | Zeroed by `SpecialEnterMap`. |
| 0x29DA | 1 | `wStatusFlags4` | mixed ✓ | Got Lapras, got starter, healed once: state. The rest is in flight. |
| 0x29DB | 1 | — | gap | |
| 0x29DC | 1 | `wStatusFlags5` | in flight ✓ | |
| 0x29DD | 1 | — | gap | |
| 0x29DE | 1 | `wStatusFlags6` | in flight ✓ | Bit 1 is the unused debug-mode bit. |
| 0x29DF | 1 | `wStatusFlags7` | in flight ✓ | |
| 0x29E0 | 1 | `wElite4Flags` | in flight ✓ | "Beat Lorelei *this run*"; reset in the Indigo lobby. |
| 0x29E1 | 1 | — | gap | |
| 0x29E2 | 1 | `wMovementFlags` | in flight ✓ | |
| 0x29E3 | 2 | `wCompletedInGameTradeFlags` | state ✓ | |
| 0x29E5 | 2 | — | gap | |
| 0x29E7 | 1 | `wWarpedFromWhichWarp` | in flight ✓ | |
| 0x29E8 | 1 | `wWarpedFromWhichMap` | in flight ✓ | |
| 0x29E9 | 2 | — | gap | |
| 0x29EB | 2 | `wCardKeyDoorY`, `X` | in flight ✓ | Zeroed on every map entry. |
| 0x29ED | 2 | — | gap | |
| 0x29EF | 2 | `wFirstLockTrashCanIndex`, `wSecondLockTrashCanIndex` | in flight ✓ | Lt. Surge's puzzle; survives a save. |
| 0x29F1 | 2 | — | gap | |
| 0x29F3 | 320 | `wEventFlags` | state ✓ | 2560 bits. |
| 0x2B33 | 293 | the wild/link union | union | See below. |
| 0x2C58 | 66 | `wEnemyMonOT` | in flight | Last opponent's party, continued. |
| 0x2C9A | 66 | `wEnemyMonNicks` | in flight | |
| 0x2CDC | 2 | `wTrainerHeaderPtr` | in flight | |
| 0x2CDE | 6 | — | gap | |
| 0x2CE4 | 1 | `wOpponentAfterWrongAnswer` | in flight | Cinnabar Gym quiz. |
| 0x2CE5 | 1 | `wCurMapScript` | in flight ✓ | |
| 0x2CE6 | 7 | — | gap | |
| 0x2CED | 5 | `wPlayTime{Hours,Maxed,Minutes,Seconds,Frames}` | state ✓ | |
| 0x2CF2 | 1 | `wSafariZoneGameOver` | in flight ✓ | |
| 0x2CF3 | 1 | `wNumSafariBalls` | in flight ✓ | |
| 0x2CF4 | 1 | `wDayCareInUse` | state ✓ | |
| 0x2CF5 | 11 | `wDayCareMonName` | state ✓ | |
| 0x2D00 | 11 | `wDayCareMonOT` | state ✓ | |
| 0x2D0B | 33 | `wDayCareMon` | state (1/33 ✓) | A full `box_struct`. Cinnabar reads only the species byte. |

**The union at 0x2B33, and the project's name.**
One branch holds the current map's wild encounter tables: `wGrassRate`, `wGrassMons` (19), 8 gap bytes, `wWaterRate`, `wWaterMons` (19).
The other holds link-battle data: `wLinkEnemyTrainerName` (11), a gap byte, 9 bytes of serial header, then `wEnemyPartyCount`, `wEnemyPartySpecies` (7) and `wEnemyMons` (6 × 44).
The enemy-party half is filled for every trainer battle, so a save usually carries the last opponent's full party.
During the Old Man's catching tutorial, the game parks your name in `wLinkEnemyTrainerName`, which is the same address as `wGrassRate`.
`LoadWildData` refills `wGrassMons` only for maps that have grass data, and Cinnabar Island has none, so there the buffer keeps the name bytes.
On the island's east coast the half-blocks are "left shore": water in the bottom-right tile, land in the bottom-left.
`TryDoWildEncounter` rolls the encounter chance from the bottom-right tile, so it uses the water rate, but picks the species list from the bottom-left tile, so it reads `wGrassMons`: your name, read as level/species pairs.
The comments in pokered's `engine/battle/core.asm` and `engine/battle/wild_encounters.asm` walk through both halves.
The MissingNo. glitch, the coast this project is named for, lives in a region we had filed under "not parsed".

**Yellow's Pikachu block (0x26DC–0x275B).**

| Offset | Size | Label | Category |
|---|---|---|---|
| 0x26DC | 8 | `wPikachuOverworldStateFlags`, `SpawnState`, `wd431`–`wd433`, `CollisionCounter`, `wd435`, `FollowCommandBufferSize` | in flight |
| 0x26E4 | 16 | `wPikachuFollowCommandBuffer` | in flight |
| 0x26F4 | 2 | `wExpressionNumber`, `wPikaPicAnimNumber` | in flight |
| 0x26F6 | 4 | `wPikachuMovementScriptBank`, `Address`, `MovementFlags` | in flight |
| 0x26FA | 34 | union: current movement data / pic-animation state | union, in flight |
| 0x271C | 1 | `wPikachuHappiness` | state |
| 0x271D | 1 | `wPikachuMood` | state |
| 0x271E | 2 | `wPikachuSpawnStateFlags`, `wd472` | in flight |
| 0x2720 | 1 | — | gap |
| 0x2721 | 1 | `wd474` | unknown |
| 0x2722 | 4 | — | gap |
| 0x2726 | 1 | `wd479` | unknown |
| 0x2727 | 24 | — | gap |
| 0x273F | 1 | `wPikachuMapScriptFlags` | in flight |
| 0x2740 | 1 | — | gap |
| 0x2741 | 2 | `wSurfingMinigameHiScore` | state (little-endian BCD) |
| 0x2743 | 1 | — | gap |
| 0x2744 | 1 | `wPrinterSettings` | setting |
| 0x2745 | 4 | `wUnknownSerialFlag_d499`, `wPrinterConnectionOpen`, `wPrinterOpcode`, `wPikachuEmotionModifier` | in flight |
| 0x2749 | 19 | — | gap |

Yellow also names two map-script slots that Red leaves as `ds 1` (`wPokemonFanClubCurScript`, `wCeladonCityCurScript`).

### Sprite Data (0x2D2C–0x2F2B)

Two arrays of 16 × 16-byte structs: `wSpriteStateData1` (picture, movement status, image index, screen position, animation counters, facing) and `wSpriteStateData2` (walk counter, displacement, grid position, movement bytes, grass priority, delay).
Slot 0 is the player; in Yellow slot 15 is Pikachu.
On Continue, slot 0 is reset.
Slots 1–15 are reused as saved, because the zero-and-reload loop sits after `LoadMapHeader`'s early return, so NPCs resume where they stood.
Category: map cache for the NPC slots, rebuilt for the player.

### Party (0x2F2C) and Current Box (0x30C0)

Party: count, 6 species + 0xFF, 6 × 44-byte `party_struct`, 6 OT names, 6 nicknames.
Box: count, 20 species + 0xFF, 20 × 33-byte `box_struct`, 20 OT names, 20 nicknames.

The current box in bank 1 is the live copy.
Deposit and withdraw only touch it; the game writes it back to its bank-2/3 slot only inside `ChangeBox`.
After any deposit since the last box change, the bank-1 copy and the slot differ; that is the normal state, not corruption.
An edit must target the bank-1 copy for the current box, because the slot copy gets overwritten from bank 1 at the next box change.

## Banks 2 and 3 (0x4000–0x7FFF): boxes 1–12

| Offset | Size | Label |
|---|---|---|
| 0x4000 | 6 × 0x462 | `sBox1`–`sBox6` |
| 0x5A4C | 1 | `sBank2AllBoxesChecksum` (over 0x4000–0x5A4B) |
| 0x5A4D | 6 | `sBank2IndividualBoxChecksums` |
| 0x5A53 | 0x5AD | gap |
| 0x6000 | 6 × 0x462 | `sBox7`–`sBox12` |
| 0x7A4C | 1 | `sBank3AllBoxesChecksum` |
| 0x7A4D | 6 | `sBank3IndividualBoxChecksums` |
| 0x7A53 | 0x5AD | gap |

Nothing initializes these banks at New Game.
The first time the player changes boxes, bit 7 of `wCurrentBoxNum` is clear, so `ChangeBox` calls `EmptyAllSRAMBoxes`: it writes count 0 and terminator 0xFF into all twelve boxes, recomputes both banks' checksums, and sets bit 7.
Until then, banks 2 and 3 hold whatever was there before: 0xFF after the title-screen clear (`ClearAllSRAMBanks`), or a previous playthrough's boxes.
An editor that writes Pokémon into a stored box while bit 7 is clear has them wiped the first time the player changes boxes.

## Corrections this surfaces in cinnabar

- **Yellow Pikachu offsets are one byte early.** `Layout.hs` has happiness 0x271B, mood 0x271C, surfing hi-score 0x2740, printer 0x2743. pokeyellow says 0x271C, 0x271D, 0x2741, 0x2744. Commit 66f24be moved happiness from the correct 0x271C to 0x271B by converting `wPikachuHappiness` (D46F) with Red's difference 0xAD54 instead of Yellow's 0xAD53. The test only checks `Just _`, and `test/data/yellow.sav` isn't in the repo, so nothing caught it.
- **The daycare Pokémon is 33 bytes**, not one species byte. Level, moves, DVs, stat experience and OT ID are all there, in the same `box_struct` the box parser already handles.
- **`ActiveBoxDesync` warns about the normal state.** Per `ChangeBox`, the slot copy is stale after any deposit or withdraw. The interesting states are "bit 7 clear, stored boxes uninitialized" and "slot differs from live copy, live copy wins".
- **Field widths from Bulbapedia:** the current box number is 1 byte plus a gap, not 2; the fossil result is 1 byte plus 2 gap bytes, not 3; 0x3522 is `hTileAnimations`, not a tileset type.
- **"Transient" mixes four categories.** `RawTransientState` holds a setting (letter delay), map cache (music), in-flight state (boulder, warps, trash cans), and the Flash palette offset. They differ in exactly what an editor needs to know.

## Open questions

- Where `ds $598` at the top of bank 1 comes from. The size matches bank 0's sprite buffers plus padding, and nothing reads it.
- `wd474` and `wd479` in Yellow have no known meaning in pokeyellow.
- Japanese layouts: `NAME_LENGTH` 6, 30 Pokémon per box, a different box and bank arrangement. pret's Western disassemblies don't cover them, so they need another source.
