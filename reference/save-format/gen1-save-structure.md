# Generation I Save Data Structure

Source: Bulbapedia — Save data structure (Generation I)

Note: These values apply to the North American Pokemon Red, Blue, and Yellow games. They could be different for other releases.

## Banks Overview

The 32 KiB save data, equal to 0x8000 bytes, is divided into 4 banks, each 8 KiB in size, equal to 0x2000 bytes.

| Bank | Offset | Name |
|------|--------|------|
| 0 | 0x0000 | Scratch and Hall of Fame |
| 1 | 0x2000 | Main |
| 2 | 0x4000 | Boxes 1-6 |
| 3 | 0x6000 | Boxes 7-12 |

**Bank 0** - Consists of 3 sprite decompression buffers which contain misc or varying data and the Hall of Fame because of its size (4,800 bytes).

**Bank 1** - The primary bank for just about all data across the game, most is directly loaded into the in-game memory byte-by-byte. It contains hundreds of variables including a full byte-by-byte copy of the current PC box.

**Bank 2** - Storage for PC boxes 1-6.

**Bank 3** - Storage for PC boxes 7-12.

## Loading

The game only mainly focuses on bank 1, and much of its contents are directly loaded byte-by-byte directly into WRAM at different addresses; as such, different offsets can be applied to convert from WRAM to the save data and back. Those are provided where possible.

Other banks are loaded either on certain game states, on certain events, or as needed. Upon switching Pokemon boxes, the contents of the new bank are copied to bank 1 from either bank 2 or 3. Events that use Hall of Fame data cause either one of or the latest Hall of Fame record to be loaded into WRAM.

## Checksum

The checksum in Generation I is only 8 bits and has a single copy. If this value is incorrect, the error message "The file data is destroyed!" will appear after the title screen, and the continue option will not appear in the menu.

The algorithm:
- Initialize the checksum to 0
- For every byte from 0x2598 to 0x3522, inclusive, add its value to the checksum
- Invert the bits of the result

Equivalent alternative:
- Initialize the checksum to 255
- For every byte from 0x2598 to 0x3522, inclusive, subtract its value from the checksum

The checksum validates the data it encompasses, but there are several checksums for different sections of data in different banks, described below.

## Bank 0

The Hall of Fame and scratch bank. None of the data in this bank is validated or checked for corruption.

The bank starts at address 0x0000 and is 0x2000 in size.

| Offset | Size | Contents |
|--------|------|----------|
| 0x0000 | 0x188 | Sprite Buffer 0 |
| 0x0188 | 0x188 | Sprite Buffer 1 |
| 0x0310 | 0x188 | Sprite Buffer 2 |
| 0x0498 | 0x100 | Unused |
| 0x0598 | 0x12C0 | Hall of Fame |
| 0x1858 | 0x7A8 | Unused |

**Scratch Buffers:** During gameplay, the game sometimes uses these as extra graphical buffers. They can safely be cleared, although they are used occasionally and anything written will be overwritten.

**Unused:** Data that the game completely does not touch. The only way to wipe these areas is by invoking the hidden "Clear Save File" dialog at the title screen.

**Hall of Fame:** Refer to the Hall of Fame data structure in the Data Types section.

## Bank 1

The prime bank containing all information needed in gameplay. Bank 2 and 3 pull data into Bank 1 when needed or are occasionally updated.

Most sections in this bank directly correspond byte-by-byte to their counterparts in in-game memory, meaning you can apply a simple offset to translate the save data address to/from the in-game memory address. Unused areas in such sections will be loaded, available in-game, and saved back.

The bank starts at address 0x2000 and is 0x2000 in size.

| Offset | Size | Contents | Copied To | Diff |
|--------|------|----------|-----------|------|
| 0x2000 | 0x598 | Unused | | |
| 0x2598 | 0xB | Player Name | 0xD158 | 0xABC0 |
| 0x25A3 | 0x789 | Main Data | 0xD2F7 | 0xAD54 |
| 0x2D2C | 0x200 | Sprite Data | 0xC100 | 0x93D4 |
| 0x2F2C | 0x194 | Party Data | 0xD163 | 0xA237 |
| 0x30C0 | 0x462 | Current Box Data | 0xDA80 | 0xA9C0 |
| 0x3522 | 0x1 | Tileset Type | 0xD0D4 | 0x9BB2 |
| 0x3523 | 0x1 | Main Data Checksum | | |
| 0x3524 | 0xADC | Unused | | |

**Player Name:** Even though the game allocates space for 10 characters + Terminator, the in-game prompt only allows typing 7.

**Main Data:** Contains most of the entire memory state from WRAM, copied byte-by-byte.

**Sprite Data:** Contains detailed sprite information in 2 sections for the map, copied byte-by-byte. There are also 2 other areas with more sprite information for the current map located in Main Data.

**Party Data:** Contains information on the player's current party, copied byte-by-byte.

**Current Box Data:** A byte-by-byte copy of the current PC box from either bank 2 or 3, copied to memory on each load. The game only deals with 1 PC box in-memory at any time.

**Main Data Checksum:** Calculated from Player Name through Tileset Type.

### Main Data

All elements inside Main Data, one of the most comprehensive areas in the save data. To translate the offset to in-game memory, use the difference 0xAD54.

| Offset | Size | Contents |
|--------|------|----------|
| 0x25A3 | 0x13 | Pokedex Owned |
| 0x25B6 | 0x13 | Pokedex Seen |
| 0x25C9 | 0x2A | Bag Items |
| 0x25F3 | 0x3 | Money |
| 0x25F6 | 0xB | Rival Name |
| 0x2601 | 0x1 | Game Options |
| 0x2602 | 0x1 | Badges |
| 0x2603 | 0x1 | Padding |
| 0x2604 | 0x1 | Letter Delay |
| 0x2605 | 0x2 | Player ID |
| 0x2607 | 0x1 | Music ID |
| 0x2608 | 0x1 | Music Bank |
| 0x2609 | 0x1 | Contrast ID |
| 0x260A | 0x1 | Current Map |
| 0x260B | 0x2 | UL Corner Cur View Tile Block Map Ptr |
| 0x260D | 0x1 | Y Coord |
| 0x260E | 0x1 | X Coord |
| 0x260F | 0x1 | Y Block Coord |
| 0x2610 | 0x1 | X Block Coord |
| 0x2611 | 0x1 | Last Map |
| 0x2612 | 0x1 | Unused |
| 0x2613 | 0x1 | Current Tileset |
| 0x2614 | 0x1 | Map Height Blocks |
| 0x2615 | 0x1 | Map Width Block |
| 0x2616 | 0x2 | Map Data Pointer |
| 0x2618 | 0x2 | Map Text Pointer |
| 0x261A | 0x2 | Map Script Pointer |
| 0x261C | 0x1 | Map Connections |
| 0x261D | 0xB | Map Connections North |
| 0x2628 | 0xB | Map Connections South |
| 0x2633 | 0xB | Map Connections West |
| 0x263E | 0xB | Map Connections East |
| 0x2649 | 0xB | Sprite Set IDs |
| 0x2654 | 0x1 | Sprite Set ID |
| 0x2655 | 0x4 | Object Data Pointers Tmp |
| 0x2659 | 0x1 | Out of Bounds Tile |
| 0x265A | 0x1 | Warp Count |
| 0x265B | 0x80 | Warp Entries |
| 0x26DB | 0x1 | Warp Destination ID |
| 0x271C | 0x1 | Pikachu Friendship (Yellow only) |
| 0x275B | 0x80 | Padding |
| 0x275C | 0x1 | Sign Count |
| 0x275D | 0x20 | Sign Coords |
| 0x277D | 0x10 | Sign Text IDs |
| 0x278D | 0x1 | Sprite Count |
| 0x278E | 0x1 | Y Offset since last special warp |
| 0x278F | 0x1 | X Offset since last special warp |
| 0x2790 | 0x20 | Sprite Data |
| 0x27B0 | 0x20 | Sprite Extra Data |
| 0x27D0 | 0x1 | Map 2x2 Meta Height |
| 0x27D1 | 0x1 | Map 2x2 Meta Width |
| 0x27D2 | 0x2 | Map View VRAM Pointer |
| 0x27D4 | 0x1 | Player Movement Direction |
| 0x27D5 | 0x1 | Player Last Stop Direction |
| 0x27D6 | 0x1 | Player Direction |
| 0x27D7 | 0x1 | Tileset Bank |
| 0x27D8 | 0x2 | Tileset Block Pointer |
| 0x27DA | 0x2 | Tileset GFX Pointer |
| 0x27DC | 0x2 | Tileset Collision Pointer |
| 0x27DE | 0x3 | Tileset Talking over Tiles |
| 0x27E1 | 0x5 | Tileset Grass Tiles |
| 0x27E6 | 0x68 | Box Items |
| 0x284C | 0x2 | Current Box Number |
| 0x284E | 0x1 | Hall of Fame Record Count |
| 0x284F | 0x1 | Unused |
| 0x2850 | 0x2 | Slot Coins |
| 0x2852 | 0x20 | Missable Objects |
| 0x2872 | 0x7 | Padding |
| 0x2879 | 0x1 | Scratch |
| 0x287A | 0x22 | Missable List |
| 0x289C | 0x100 | Completed Scripts |
| 0x299C | 0xE | Owned Hidden Items |
| 0x29AA | 0x2 | Owned Hidden Coins |
| 0x29AC | 0x1 | Walking, Biking, or Surfing |
| 0x29AD | 0x10 | Padding |
| 0x29B7 | 0x2 | Towns visited |
| 0x29B9 | 0x2 | Safari Steps |
| 0x29BB | 0x1 | Fossil Item Given ID |
| 0x29BC | 0x3 | Fossil Pokemon Result ID |
| 0x29BF | 0x1 | Enemy Pokemon or Trainer class |
| 0x29C0 | 0x1 | Player Jumping Y Screen Coords |
| 0x29C1 | 0x1 | Rival first partner Pokemon |
| 0x29C2 | 0x1 | Padding |
| 0x29C3 | 0x1 | Player first partner Pokemon |
| 0x29C4 | 0x1 | Boulder Sprite Index |
| 0x29C5 | 0x1 | Last Blackout Map |
| 0x29C6 | 0x1 | Destination Map |
| 0x29C7 | 0x1 | Unused |
| 0x29C8 | 0x1 | Tile in front of Boulder or Collision |
| 0x29C9 | 0x1 | Dungeon Warp Destination |
| 0x29CA | 0x1 | Which Dungeon Warp Used |
| 0x29CB | 0x9 | Unused |
| 0x29D4 | 0x1 | Various Flags 1 |
| 0x29D5 | 0x1 | Padding |
| 0x29D6 | 0x1 | Defeated Gyms |
| 0x29D7 | 0x1 | Padding |
| 0x29D8 | 0x1 | Various Flags 2 |
| 0x29D9 | 0x1 | Various Flags 3 |
| 0x29DA | 0x1 | Various Flags 4 |
| 0x29DB | 0x1 | Padding |
| 0x29DC | 0x1 | Various Flags 5 |
| 0x29DD | 0x1 | Padding |
| 0x29DE | 0x1 | Various Flags 6 |
| 0x29DF | 0x1 | Various Flags 7 |
| 0x29E0 | 0x2 | Defeated Lorelei |
| 0x29E2 | 0x1 | Various Flags 8 |
| 0x29E3 | 0x2 | In-Game Trades |
| 0x29E5 | 0x1 | Padding |
| 0x29E7 | 0x1 | Warped from Warp |
| 0x29E8 | 0x1 | Warped from Map |
| 0x29E9 | 0x2 | Padding |
| 0x29EB | 0x1 | Card key door Y |
| 0x29EC | 0x1 | Card key door X |
| 0x29ED | 0x2 | Padding |
| 0x29EF | 0x1 | First Trash Can Lock |
| 0x29F0 | 0x1 | Second Trash Can Lock |
| 0x29F1 | 0x2 | Padding |
| 0x29F3 | 0x140 | Completed Game Events |
| 0x2B33 (U) | 0x1 | Grass Rate |
| 0x2B33 (U) | 0x1 | Link Trainer |
| 0x2B34 | 0xB | Grass Pokemon |
| 0x2B3F (U) | 0x9 | Grass Pokemon |
| 0x293F (U) | 0x9 | Link Data |
| 0x2B48 | 0x1 | Enemy Party Count |
| 0x2B49 | 0x7 | Enemy Party Pokemon |
| 0x2B50 (U) | 0x1 | Water Rate |
| 0x2B51 (U) | 0x1 | Water Pokemon |
| 0x2B50 (U) | 0x1 | Enemy Partial Party Data |
| 0x2CDC | 0x2 | Trainer Header Pointer |
| 0x2CDE | 0x6 | Padding |
| 0x2CE4 | 0x1 | Opponent ID after wrong answer |
| 0x2CE5 | 0x1 | Current Map Script |
| 0x2CE6 | 0x7 | Padding |
| 0x2CED | 0x1 | Play Time Hours |
| 0x2CEE | 0x1 | Play Time Maxed |
| 0x2CEF | 0x1 | Play Time Minutes |
| 0x2CF0 | 0x1 | Play Time Seconds |
| 0x2CF1 | 0x1 | Play Time Frames |
| 0x2CF2 | 0x1 | Safari Game Over |
| 0x2CF3 | 0x1 | Safari Ball Count |
| 0x2CF4 | 0x1 | Daycare In-Use |
| 0x2CF5 | 0xB | Daycare Pokemon Name |
| 0x2D00 | 0xB | Daycare Original Trainer |
| 0x2D0B | 0x1 | Daycare Pokemon |

(U) marks union fields: shared space where different variables overlap at the same offset depending on context.

#### Field descriptions

**Padding:** Extra space assigned to an area in memory. The space is larger than the variables make up. It's unknown if padding is ever used; it could be used in various game unions with shared space.

**Pokedex Seen/Owned:** Pokemon are indexed by Pokedex order (same as National Pokedex), 0-based. 1 bit per Pokemon. Bits ordered within bytes from lowest to highest:

`Bit = Data[PokeNum >> 3] >> (PokeNum & 7) & 1;`

Entry #152: As the bit lists are packed into 19 bytes, there is space for 152 entries. The last entry (bit 7 of byte 18) does represent an entry #152, but it is simply a copy of Kangaskhan.

**Bag Items:** Follows Item List data structure with a capacity of 20.

**Money:** Binary Coded Decimal, 6-digit number, 2 digits per byte, big-endian. Interpreted as decimal even if viewed as hexadecimal; don't use hex A-F.

**Rival Name:** Allocates space for 10 characters + Terminator, but the in-game prompt only allows typing 7.

**Game Options:** Stored in one byte:
- bit 7 (MSB): battle effects ('1' for No, '0' for Yes)
- bit 6: battle style ('1' for Set, '0' for Switch)
- bit 4: sound ('0' for Mono, '1' for Stereo)
- bit 2-0: text speed ('001' for Fast, '011' for Normal, '101' for Slow)

In Pokemon Yellow:
- bit 5-4: sound ('00' for Mono, '01' for Earphone1, '10' for Earphone2, '11' for Earphone3)

**Badges:** Eight bits, one per badge; '1' = acquired, '0' = not. MSB to LSB: Boulder, Cascade, Thunder, Rainbow, Soul, Marsh, Volcano, Earth.

**Letter Delay:**
- bit 0: If 0, limit the delay to 1 frame. No effect if delay disabled through bit 1 or bit 6 of Various Flags 5.
- bit 1: If 0, no delay.

**Pikachu Friendship:** Friendship level of the partner Pikachu. Unused in Red and Blue.

**Player ID:** Randomly generated 16-bit ID for the player and their Pokemon, most used in trades.

**Music ID:** Which music or sound plays in this map.

**Music Bank:** Which bank the music or sound is in.

**Contrast ID:** Offset subtracted from FadePal4 to get the background and object palettes. Normally 0; set to 6 when Flash is needed (causing FadePal2 to be used instead of FadePal4).

**Current Map:** Current Map ID the player was last at.

**UL Corner Cur View Tile Block Map Ptr:** Pointer to the upper left corner of the current view in the tile block map.

**X/Y Coord:** Player X and Y coordinates.

**X/Y Block Coord:** Same as X/Y Coord but in blocks.

**Last Map:** Last map the player visited.

**Current Tileset:** Current tileset in map.

**Map Height/Width Blocks:** Height and width of the map in blocks.

**Map Data/Text/Script Pointers:** Pointers to various code for the current map.

**Map Connections:** A bit field describing the connections for the current map. If there are connecting maps, the details will be in the specific connection data (see Map Connection Entry in Data Types).

**Sprite Set IDs:** Sprite set for the current map (11 sprite picture IDs), followed by Sprite Set ID for the current map.

**Out of Bounds Tile:** The tile shown outside the boundaries of the map.

**Warp Count:** Number of warps on the current map.

**Warp Entries:** List of warp entries on the current map.

**Warp Destination ID:** If 0xFF, the player's coordinates are not updated when entering the map.

**Sign Count:** Number of signs on the map, up to 16.

**Sign Coords:** Follows sign coordinate data structure, up to 16 signs.

**Sign Text IDs:** Text ID of each sign, 16 bytes (1 byte per sign).

**Sprite Count:** Number of sprites on the current map.

**X/Y Offset since last special warp:** Track X and Y offset in blocks from the last special warp used; don't seem to be used for anything.

**Sprite Data/Extra:** Various sprite data for up to 16 sprites, follows sprite data structures.

**Map 2x2 Meta Height and Width:** Map height and width in 2x2 meta tiles.

**Map View VRAM Pointer:** Address of the upper left corner of the visible portion of the BG tile map in VRAM.

**Player Movement Direction:** If the player is moving, the current direction. If not moving, zero. Map scripts write to this to change facing direction.

**Player Last Stop Direction:** The direction the player was moving before last stopping.

**Player Direction:** If moving, the current direction; if not moving, the last direction moved.

**Tileset Bank:** Bank current tileset can be found in.

**Tileset Block Pointer:** Maps blocks (4x4 tiles) to tiles.

**Tileset Collision Pointer:** List of all walkable tiles.

**Tileset Talking over Tiles:** Tiles between you and an NPC where you can still interact even though you're not directly in front of them.

**Tileset Grass Tiles:** Presumably tiles where wild Pokemon can be found (grass, mansion floor, cave floor, etc.).

**Box Items:** Items in the PC's item box, follows item box data structure with a capacity of 50.

**Current Box Number:**
- bits 0-6: box number (box 1 = 0, box 12 = 11)
- bit 7: whether the player has changed boxes before

**Hall of Fame Record Count:** How many Hall of Fame victories in this save file. Up to 50 before it starts erasing the oldest records.

**Slot Coins:** Binary Coded Decimal, 4-digit number, 2 digits per byte, big-endian.

**Missable Objects:** Bit array of missable objects. Set = removed.

**Scratch:** Temp copy of c1x2 (sprite facing/anim).

**Missable List:** Follows the missable list data structure.

**Completed Scripts:** Long array of completed scripts for all maps and some NPCs.

**Owned Hidden Items/Coins:** Bit array of hidden items and coins player has found.

**Walking, Biking, or Surfing:** Current movement mode.

**Towns visited:** Bit array of towns player has ever visited.

**Safari Steps:** Number of safari zone steps taken so far. Starts at 502.

**Fossil Given and Result:** Item given to Cinnabar lab and Pokemon that will result.

**Enemy Pokemon or Trainer class:** Shared space: Enemy Pokemon ID in wild battles, Trainer class in trainer battles.

**Rival/Player first partner Pokemon:** Pokemon each picked.

**Boulder Sprite Index:** Sprite index of the boulder the player is trying to push.

**Destination Map:** For certain types of special warps, not ordinary walking.

**Tile in front of Boulder or Collision:** Tile in front of boulder when pushing. Also stores collision check result (0xFF = collision, 0x00 = no collision).

**First/Second Trash Can Lock:** Lt. Surge's trash can lock puzzle switch locations.

**Completed Game Events:** Bitfield of game events completed.

**Grass Rate:** Wild Pokemon encounter rate for "grass" areas.

**Grass Pokemon:** 20-byte list of wild Pokemon from most common to rarest. Last 9 bytes cross over into shared space used for link battles.

**Enemy Party Count:** How many Pokemon the enemy has.

**Enemy Party Pokemon:** Species IDs (7 bytes).

**Water Rate:** Encounter rate for water areas.

**Opponent ID after wrong answer:** Trainer the player must face after a wrong answer in the Cinnabar gym quiz.

**Current Map Script:** Index of current map script, mostly used as index for function pointer array.

**Playtime:** Hours, Minutes, Seconds, and Frames. Includes a byte that determines whether the counter has hit maximum. Maximum time: 255 Hours, 59 Minutes, 59 Seconds, and 59 Frames (10 days, 15 hours, 59 minutes, 59 seconds, 59 frames). The maxed byte can be any nonzero number to disable the counter; the game sets it to 255 when maxed. Setting Minutes, Seconds, or Frames to 60+ will only increment the next byte by 1. The timer is always ticking, even on the initial load/new game menu or when paused.

**Daycare:** 4 variables: In-Use flag, Pokemon Name, Original Trainer name, and Pokemon (uses the Box Data format alone).

#### Various Flags 1

- bit 0: using Strength outside of battle
- bit 1: set by IsSurfingAllowed when surfing's allowed, but the caller resets it after checking
- bit 3: received Old Rod
- bit 4: received Good Rod
- bit 5: received Super Rod
- bit 6: gave one of the Saffron guards a drink
- bit 7: set by ItemUseCardKey, leftover code from a previous Card Key implementation

#### Defeated Gyms

Redundant because it matches Earned Badges; used to determine whether to show name on statue and in two NPC text scripts.

#### Various Flags 2

- Bit 0: if not set, the 3 minimum steps between random battles have passed
- Bit 1: prevent audio fade out

#### Various Flags 3

Used for temporary flags and as the destination map when warping to the Trade Center or Colosseum.

- bit 0: sprite facing directions have been initialised in the Trade Center
- bit 3: do scripted warp (used to warp back to Lavender Town from the top of the Pokemon tower)
- bit 4: on a dungeon warp
- bit 5: don't make NPCs face the player when spoken to
- bits 6-7: set by scripts when starting major battles in the storyline, but they do not appear to affect anything. Bit 6 is reset after all battles and bit 7 is reset after trainer battles (but it's only set before trainer battles anyway).

#### Various Flags 4

- bit 0: the player has received Lapras in Silph Co.
- bit 1: set in various places, but doesn't appear to have an effect
- bit 2: the player has healed Pokemon at a Pokemon center at least once
- bit 3: the player has received a Pokemon from Prof. Oak
- bit 4: disable battles
- bit 5: set when a battle ends and when the player blacks out in the overworld due to poison
- bit 6: using the link feature
- bit 7: set if scripted NPC movement has been initialised

#### Various Flags 5

- bit 0: NPC sprite being moved by script
- bit 5: ignore joypad input
- bit 6: print text with no delay between each letter
- bit 7: set if joypad states are being simulated in the overworld or an NPC's movement is being scripted

#### Various Flags 6

- bit 0: play time being counted
- bit 1: remnant of debug mode? Not set by the game code. If set: 1) skips most of Prof. Oak's speech, uses NINTEN as player's name and SONY as rival's name; 2) does not start player on floor two of player's house (sends to Last Map instead); 3) allows wild battles to be avoided by holding down B
- bit 2: the target warp is a fly warp (bit 3 set or blacked out) or a dungeon warp (bit 4 set)
- bit 3: used warp pad, escape rope, dig, teleport, or fly, so the target warp is a "fly warp"
- bit 4: jumped into hole (Pokemon Mansion, Seafoam Islands, Victory Road) or went down waterfall (Seafoam Islands), so the target warp is a "dungeon warp"
- bit 5: currently being forced to ride bike (cycling road)
- bit 6: map destination is Last Blackout Map (usually the last used Pokemon center, but could be the player's house)

#### Various Flags 7

- bit 0: running a test battle
- bit 1: prevent music from changing when entering new map
- bit 2: skip the joypad check in CheckWarpsNoCollision (used for the forced warp down the waterfall in Seafoam Islands)
- bit 3: trainer wants to battle
- bit 4: use variable Current Map Script instead of the provided index for next frame's map script (used to start battle when talking to trainers)
- bit 7: used fly out of battle

#### Defeated Lorelei

- Bit 1: set when you beat Lorelei and reset in Indigo Plateau lobby. The game uses this to tell when Elite 4 events need to be reset, enforcing the requirement that the Elite 4 and Champion be defeated in a row with no losses.

#### Various Flags 8

- bit 0: check if the player is standing on a door and make him walk down a step if so
- bit 1: the player is currently stepping down from a door
- bit 2: standing on a warp
- bit 6: jumping down a ledge / fishing animation
- bit 7: player sprite spinning due to spin tiles (Rocket hideout / Viridian Gym)

#### In-Game Trades

Bitset of completed in-game trades.

#### Completed Scripts

| Offset | Size | Contents |
|--------|------|----------|
| 0x289C | 0x1 | Oaks Lab |
| 0x289D | 0x2 | Palette Town |
| 0x289F | 0x1 | Rival's House |
| 0x28A0 | 0x3 | Viridian City |
| 0x28A3 | 0x1 | Pewter City |
| 0x28A4 | 0x1 | Route 3 |
| 0x28A5 | 0x2 | Route 4 |
| 0x28A7 | 0x1 | Viridian Gym |
| 0x28A8 | 0x1 | Pewter Gym |
| 0x28A9 | 0x1 | Cerulean Gym |
| 0x28AA | 0x1 | Vermilion Gym |
| 0x28AB | 0x1 | Celadon Gym |
| 0x28AC | 0x1 | Route 6 |
| 0x28AD | 0x1 | Route 8 |
| 0x28AE | 0x1 | Route 24 |
| 0x28AF | 0x1 | Route 25 |
| 0x28B0 | 0x1 | Route 9 |
| 0x28B1 | 0x1 | Route 10 |
| 0x28B2 | 0x1 | Mt. Moon 1 |
| 0x28B3 | 0x1 | Mt. Moon 3 |
| 0x28B4 | 0x1 | S.S. Anne 8 |
| 0x28B5 | 0x1 | S.S. Anne 9 |
| 0x28B6 | 0x2 | Route 22 |
| 0x28B8 | 0x1 | Player's House 2 |
| 0x28B9 | 0x1 | Viridian Market |
| 0x28BA | 0x1 | Route 22 Gate |
| 0x28BB | 0x8 | Cerulean City |
| 0x28C3 | 0x1 | S.S. Anne 5 |
| 0x28C4 | 0x1 | Viridian Forest |
| 0x28C5 | 0x1 | Museum 1 |
| 0x28C6 | 0x1 | Route 13 |
| 0x28C7 | 0x1 | Route 14 |
| 0x28C8 | 0x1 | Route 17 |
| 0x28C9 | 0x1 | Route 19 |
| 0x28CA | 0x1 | Route 21 |
| 0x28CB | 0x1 | Safari Zone Entrance |
| 0x28CC | 0x1 | Rock Tunnel 2 |
| 0x28CD | 0x2 | Rock Tunnel 1 |
| 0x28CF | 0x1 | Route 11 |
| 0x28D0 | 0x1 | Route 12 |
| 0x28D1 | 0x1 | Route 15 |
| 0x28D2 | 0x1 | Route 16 |
| 0x28D3 | 0x1 | Route 18 |
| 0x28D4 | 0x1 | Route 20 |
| 0x28D5 | 0x1 | S.S. Anne 10 |
| 0x28D6 | 0x1 | Vermilion City |
| 0x28D7 | 0x1 | Pokemon tower 2 |
| 0x28D8 | 0x1 | Pokemon tower 3 |
| 0x28D9 | 0x1 | Pokemon tower 4 |
| 0x28DA | 0x1 | Pokemon tower 5 |
| 0x28DB | 0x1 | Pokemon tower 6 |
| 0x28DC | 0x1 | Pokemon tower 7 |
| 0x28DD | 0x1 | Rocket Hideout 1 |
| 0x28DE | 0x1 | Rocket Hideout 2 |
| 0x28DF | 0x1 | Rocket Hideout 3 |
| 0x28E0 | 0x2 | Rocket Hideout 4 |
| 0x28E2 | 0x1 | Route 6 Gate |
| 0x28E3 | 0x2 | Route 8 Gate |
| 0x28E5 | 0x1 | Cinnabar Island |
| 0x28E6 | 0x2 | Mansion 1 |
| 0x28E8 | 0x1 | Mansion 2 |
| 0x28E9 | 0x1 | Mansion 3 |
| 0x28EA | 0x1 | Mansion 4 |
| 0x28EC | 0x2 | Victory Road 3 |
| 0x28EE | 0x1 | Fighting Dojo |
| 0x28EF | 0x1 | Silph Co 2 |
| 0x28F0 | 0x1 | Silph Co 3 |
| 0x28F1 | 0x1 | Silph Co 4 |
| 0x28F2 | 0x1 | Silph Co 5 |
| 0x28F3 | 0x1 | Silph Co 6 |
| 0x28F4 | 0x1 | Silph Co 7 |
| 0x28F5 | 0x1 | Silph Co 8 |
| 0x28F6 | 0x1 | Silph Co 9 |
| 0x28F7 | 0x1 | Hall of Fame Room |
| 0x28F8 | 0x1 | Rival |
| 0x28F9 | 0x1 | Lorelei |
| 0x28FA | 0x1 | Bruno |
| 0x28FB | 0x1 | Agatha |
| 0x28FC | 0x1 | Unknown Dungeon 3 |
| 0x28FD | 0x1 | Victory Road 1 |
| 0x28FF | 0x5 | Lance |
| 0x2904 | 0x1 | Silph Co 10 |
| 0x2905 | 0x2 | Silph Co 11 |
| 0x2907 | 0x1 | Fuchsia Gym |
| 0x2908 | 0x2 | Saffron Gym |
| 0x290A | 0x1 | Cinnabar Gym |
| 0x290B | 0x1 | Celadon Game Corner |
| 0x290C | 0x1 | Route 16 Gate |
| 0x290D | 0x1 | Bill's House |
| 0x290E | 0x1 | Route 5 Gate |
| 0x290F (U) | 0x2 | Power Plant |
| 0x290F (U) | 0x2 | Route 7 Gate |
| 0x2911 | 0x1 | S.S. Anne 2 |
| 0x2912 | 0x1 | Seafoam Islands 4 |
| 0x2913 | 0x1 | Route 23 |
| 0x2914 | 0x1 | Seafoam Islands 5 |
| 0x2915 | 0x1 | Route 18 Gate |
| 0x2916 | 0x78 | Padding |
| 0x2964 | 0x56 | Completed Scripts End Padding |

### Sprite Data

Covers sprite data on map. To translate offset to in-game memory, use difference 0x93D4.

These follow the Basic Sprite Data Structure:

| Offset | Size | Contents | Amount |
|--------|------|----------|--------|
| 0x2D2C | 0x10 | Player | 1 |
| 0x2D3C | 0xF0 | Sprite | 15 |

These follow the Extra Sprite Data Structure:

| Offset | Size | Contents | Amount |
|--------|------|----------|--------|
| 0x2E2C | 0x10 | Player | 1 |
| 0x2E3C | 0xF0 | Sprite | 15 |

### Party Data

Trainer's current party. To translate offset to in-game memory, use difference 0xA237. Follows the full party data structure.

| Offset | Size | Contents |
|--------|------|----------|
| 0x2F2C | 0x194 | Party Data |

### Current Box Data

A full byte-by-byte copy of the current box, loaded directly into memory on each load. To translate offset to in-game memory, use difference 0xA9C0. Follows the full box data structure.

| Offset | Size | Contents |
|--------|------|----------|
| 0x30C0 | 0x462 | Box Data |

Normally, Pokemon are deposited and withdrawn from the Current Box data, which is within the checksum-validated region. When switching boxes, the data from the Current Box is copied to the corresponding PC Box data, then the data from the switched-to PC Box is transferred into the Current Box data.

## Bank 2

Contains boxes 1-6. When a box is selected, its contents are copied and cached in bank 1. Bank 2 and 3 are completely identical in structure.

Bank 2 and 3 have 2 different checksums: a whole-bank checksum similar to bank 1 that encompasses all used data on the bank, and 6 individual checksums for the individual boxes' data.

All Pokemon boxes follow the full box data structure.

Bank 2 starts at address 0x4000 and is 0x2000 in size.

| Offset | Size | Contents |
|--------|------|----------|
| 0x4000 | 0x462 | Pokemon Box 1 |
| 0x4462 | 0x462 | Pokemon Box 2 |
| 0x48C4 | 0x462 | Pokemon Box 3 |
| 0x4D26 | 0x462 | Pokemon Box 4 |
| 0x5188 | 0x462 | Pokemon Box 5 |
| 0x55EA | 0x462 | Pokemon Box 6 |
| 0x5A4C | 0x1 | All Checksums |
| 0x5A4D | 0x6 | Individual Checksums |
| 0x5A53 | 0x5AD | Unused |

## Bank 3

Contains boxes 7-12. Identical structure to Bank 2.

Bank 3 starts at address 0x6000 and is 0x2000 in size.

| Offset | Size | Contents |
|--------|------|----------|
| 0x6000 | 0x462 | Pokemon Box 7 |
| 0x6462 | 0x462 | Pokemon Box 8 |
| 0x68C4 | 0x462 | Pokemon Box 9 |
| 0x6D26 | 0x462 | Pokemon Box 10 |
| 0x7188 | 0x462 | Pokemon Box 11 |
| 0x75EA | 0x462 | Pokemon Box 12 |
| 0x7A4C | 0x1 | All Checksums |
| 0x7A4D | 0x6 | Individual Checksums |
| 0x7A53 | 0x5AD | Unused |

## Data Types

Unless otherwise noted, integer values occupy the specified number of bytes, and are big-endian and either unsigned or two's complement.

### Text

Text data is stored in a proprietary encoding. Fixed-length user-input strings are terminated with 0x50. If a fixed-length string is terminated before using its full capacity, the contents of the remaining space are ignored.

### Hall of Fame

The Hall of Fame is the only Pokemon data structure that's so stripped down. The game allocates space for 50 Hall of Fame Records, each containing 6 entries for each Pokemon and each Pokemon entry contains 16 bytes. In code, it only uses the first 13 bytes; the last 3 are unused/padding.

Hall of Fame Structure:

| Offset | Size | Contents | Amount |
|--------|------|----------|--------|
| 0x00 | 0x60 | Hall of Fame Records | 50 |

Hall of Fame Record Entry Structure:

| Offset | Size | Contents | Amount |
|--------|------|----------|--------|
| 0x00 | 0x10 | Pokemon | 6 |

Hall of Fame Pokemon Structure:

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Species ID |
| 0x01 | 0x1 | Level |
| 0x02 | 0xB | Pokemon Name |
| 0x0D | 0x3 | Padding |

### Lists

Lists have 3 parts: a count of list entries, the list entries themselves, and the list ending. If count is 0, the count byte is followed immediately by the terminator 0xFF.

A list entry consists of 2 bytes: the item ID and the amount (1-99).

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Count |
| 0x01 | 0x2 x Count | Entries |
| 0x01 + 2 x Count | 0x1 | End (0xFF) |

List Entry:

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Index |
| 0x01 | 0x1 | Amount |

#### Missable Lists

A variation of the standard list: instead of ID and amount, it's ID and index. There's no count byte and thus there may not be a terminator byte.

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | ID |
| 0x01 | 0x1 | Index |

### Map Connection Entry

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Map Pointer |
| 0x01 | 0x2 | Strip Source |
| 0x03 | 0x2 | Strip Destination |
| 0x05 | 0x1 | Strip Width |
| 0x06 | 0x1 | Connected Map Width |
| 0x07 | 0x1 | Connected Map Y Align |
| 0x08 | 0x1 | Connected Map X Align |
| 0x09 | 0x2 | Connected Map View Pointer |

### Sign Coordinates

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Y |
| 0x01 | 0x1 | X |

### Sprite Data Structures

Basic Sprite Data:

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Picture ID |
| 0x01 | 0x1 | Movement Status |
| 0x02 | 0x1 | Image Index |
| 0x03 | 0x1 | Y Step Vector |
| 0x04 | 0x1 | Y Pixels |
| 0x05 | 0x1 | X Step Vector |
| 0x06 | 0x1 | X Pixels |
| 0x07 | 0x1 | Intra Animation Frame Counter |
| 0x08 | 0x1 | Animation Frame Counter |
| 0x09 | 0x1 | Facing Direction |
| 0x0A | 0x6 | Unused padding |

Extra Sprite Data:

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Walk Animation Counter |
| 0x01 | 0x1 | Unused padding |
| 0x02 | 0x1 | Y Disp |
| 0x03 | 0x1 | X Disp |
| 0x04 | 0x1 | Map Y |
| 0x05 | 0x1 | Map X |
| 0x06 | 0x1 | Movement Byte |
| 0x07 | 0x1 | Grass Priority |
| 0x08 | 0x1 | Movement Delay |
| 0x09 | 0x5 | Unused Padding |
| 0x0E | 0x1 | Image Base Offset |
| 0x0F | 0x6 | Unused padding |

### Map Sprite Data

Sprite Basic Data Entry:

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Movement |
| 0x01 | 0x1 | Text ID |

Sprite Extended Data Entry:

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Trainer class or Item ID |
| 0x01 | 0x1 | Trainer Set ID |

### Pokemon Data Structures

When a Pokemon is in a PC or in the Day Care (since the Daycare follows the same PC Box data structure), it uses the base 33-byte structure containing base information needed to calculate other values (to save space).

When you withdraw a Pokemon from the PC, that information is calculated and placed in an extended 44-byte structure. This calculated information is destroyed when the Pokemon is placed back into the PC.

List types:
- Player's party: capacity 6, full Pokemon data structure (44 bytes)
- PC Box: capacity 20, truncated Pokemon data structure (first 33 bytes only)
- Daycare: single Pokemon capacity, contains only the Pokemon Data structure with none of the "list" aspects

#### Full Box Data

| Offset | Size | Contents | Amount |
|--------|------|----------|--------|
| 0x00 | 0x1 | Pokemon Count | |
| 0x01 | 0x1 | Species ID | 20 |
| 0x15 | 0x1 | Unused Padding | |
| 0x16 | 0x21 | Pokemon Box Data | 20 |
| 0x2AA | 0xB | Original Trainer Names | 20 |
| 0x386 | 0xB | Pokemon Names | 20 |

**Pokemon Count:** Number of Pokemon entries actually used.

**Species ID:** List of species indexes, 1 byte per index, with 0xFF terminator following the last used entry. Used by the party screen and PC's Box management interface.

**Pokemon Box Data:** 44 or 33 bytes per index depending on party or PC Box.

**OT Names:** 11 bytes per name, 1 to 10 characters terminated by 0x50.

**Names:** 11 bytes per name, 1 to 10 characters terminated by 0x50. A name is a nickname if it does not perfectly match the default species name (typically all uppercase) with unused bytes filled with 0x50 terminators.

#### Full Party Data

Same as Full Box but formatted for a party of up to 6.

| Offset | Size | Contents | Amount |
|--------|------|----------|--------|
| 0x00 | 0x1 | Pokemon Count | |
| 0x01 | 0x1 | Species ID | 6 |
| 0x07 | 0x1 | Unused Padding | |
| 0x08 | 0x2C | Pokemon Party Data | 6 |
| 0x110 | 0xB | Original Trainer Names | 6 |
| 0x152 | 0xB | Pokemon Names | 6 |

#### Partial Party Data

A stripped-down full party data, missing much of the upper data.

| Offset | Size | Contents | Amount |
|--------|------|----------|--------|
| 0x0 | 0x21 | Pokemon Party Data | 6 |
| 0xC6 | 0xB | Original Trainer Names | 6 |
| 0x108 | 0xB | Pokemon Names | 6 |

#### Item Box

Same rules as a regular list but space allotted for 50 items.

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 0x1 | Count |
| 0x01 | 0x2 x Count | Entries |
| 0x01 + 2 x Count | 0x1 | End (0xFF) |

## Credits

> Massive credit goes to the Pokemon Red/Blue source code (pret). Specifically the wram and sram files. Permission was directly obtained from the team and project admins for these references.
