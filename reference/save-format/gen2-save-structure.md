# Generation II Save Data Structure

Source: Bulbapedia — Save data structure (Generation II)

## Overview

The save data structure for Generation II is stored in the cartridge's battery-backed RAM chip (SRAM), or as a ".sav" file by most emulators. The structure consists of 32 KiB of data, though not every byte is used. Emulators may append additional data for the purposes of maintaining real-time clock operations.

Two regions of the save data have their integrity validated via checksums. These regions contain all of the information that directly pertains to the player and their Pokemon. Additional information pertinent to the save file is also present in the other regions of the data.

The 32 KiB save data, equal to 0x8000 bytes, is divided into 4 banks, each 8 KiB in size, equal to 0x2000 bytes.

## Data types

Unless otherwise noted, integer values occupy the specified number of bytes, and are big-endian and either unsigned or two's complement.

Text data is stored in a proprietary encoding.

## Item lists

Item lists in the save data follow a specific format.

Lists have entries of 2 bytes each as well as a capacity. The total size of the list data in bytes is capacity x 2 + 2.

For example, the player's pocket inventory can hold 20 item entries, so the size of the list is 20 x 2 + 2 = 42 bytes.

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 1 | Count |
| 0x01 | 2 x Capacity | Item entries |
| ... | 1 | Terminator |

**Count:** The number of item entries actually being represented in the list. Count and capacity are separate.

**Entries:** The exact data for each item entry in the list.

**Terminator:** The byte following the last item entry, according to Count, must always be a terminator, which is byte value 0xFF. This spare byte is present at the end of the list data to handle the list being filled to capacity.

Entry format:

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 1 | Count |
| 0x01 | 1 | Index |

**Count:** The amount of that particular item. This value must be between 1 and 99 inclusive.

**Index:** The item's index.

## Pokemon lists

Lists of Pokemon in the save data follow a particular format.

Lists have entries of varying sizes, and a capacity. The total size of the list data in bytes is (capacity x (size + 23) + 2) in a save file from an English game, and (capacity x (size + 13) + 2) in a save file from a Japanese game.

For example, the player's Pokemon team contains 6 entries and each entry is 48 bytes in size, so the size of that list is 6 x (48 + 23) + 2 = 428 bytes in an English save file, and 6 x (48 + 13) + 2 = 368 bytes in a Japanese one.

| Offset | Size | Contents |
|--------|------|----------|
| 0x0000 | 1 | Count |
| 0x0001 | Capacity + 1 | Species |
| ... | Capacity x Size | Pokemon |
| ... | Capacity x 11 | OT Names |
| ... | Capacity x 11 | Names |

**Count:** The number of Pokemon entries actually being represented in the list. A Pokemon list may have a capacity of, say, 30 Pokemon but only use a few of those slots. The count tells how many are actually used.

**Species:** A list of species indexes, one for each Pokemon in the list. This is used by the team menu as well as the PC management interface. The byte following the last species entry, according to Count, must always be a terminator (0xFF). If an entry in this field is set to 0xFD, then the corresponding Pokemon is in an Egg.

**Pokemon:** The exact data for each Pokemon entry in the list. See: Pokemon data structure (Generation II). For party Pokemon, the entry size is the full 48 bytes. For PC Pokemon, only the first 32 bytes are used; the remaining values are regenerated upon withdrawing a Pokemon from the PC. This is the basis of the Box trick.

**OT Names:** Text strings representing the names of the original Trainers for each Pokemon entry. Each name can contain up to 10 characters in an English save, and up to 5 characters in a Japanese one. Following the 10 or 5 bytes, there is always one 0xFF byte, for a total of 11 or 6 bytes per name.

**Names:** Text strings representing the names for each Pokemon entry. Same length rules as OT Names. A name is considered a "nickname" if it does not perfectly match the default name for a Pokemon. The default name rules:
- The first characters must match the species name exactly (typically all-uppercase).
- The remainder of the string must be all terminator characters (0x50).

Therefore, if a Pokemon with a 9- or 10-letter species name, such as Charmander, is given a nickname that matches the species name, the nickname will not be retained should that Pokemon evolve.

## File structure

Known data within the save file can be found at the following offsets, such that offset 0 is the first byte of an emulator ".sav" file.

Although all data appears twice in the save file, only the primary copy is documented below. For more information, see the Checksums section.

English and Japanese save files are quite a bit different, mostly due to Japanese Generation II games having 9 boxes in the PC of 30 Pokemon each, while the English games have 14 boxes of 20 each. The save format also differs between game versions, as Crystal has more features than Gold and Silver.

| English GS | English C | Eng Size | Japanese GS | Japanese C | JP Size | Contents |
|------------|-----------|----------|-------------|------------|---------|----------|
| 0x2000 | 0x2000 | 8 | 0x2000 | 0x2000 | 8 | Options |
| 0x2008 | 0x2008 | 3 | 0x2008 | 0x2008 | 3 | Player Trainer ID |
| 0x200B | 0x200B | 11 | 0x200B | 0x200B | 6 | Player name |
| 0x2016 | 0x2016 | 11 | 0x2011 | 0x2011 | 6 | Unused (Player's mom name) |
| 0x2021 | 0x2021 | 11 | 0x2017 | 0x2017 | 5 | Rival name |
| 0x202C | 0x202C | 11 | 0x201B | 0x201B | 6 | Unused (Red's name) |
| 0x2030 | 0x2030 | 5 | 0x2021 | 0x2021 | 6 | Unused (Green's name) |
| 0x2042 | 0x2042 | 1 | 0x2029 | 0x2029 | 1 | Daylight savings |
| 0x2053 | 0x2053 | 4 | 0x2034 | 0x2035 | 4 | Time played |
| 0x206B | 0x206A | 1 | 0x204C | 0x204C | 1 | Player palette |
| 0x23E2 | 0x23E3 | 2 | 0x23C3 | 0x23C5 | 2 | Game Coins |
| 0x23DB | 0x23DC | 3 | 0x23BC | 0x23BE | 3 | Money |
| 0x23E4 | 0x23E5 | 1 | 0x23C5 | 0x23C7 | 1 | Johto Badges |
| 0x23E5 | 0x23E6 | 1 | 0x23C6 | 0x23C8 | 1 | Kanto Badges |
| 0x23E6 | 0x23E7 | 57 | 0x23C7 | 0x23C9 | 57 | TM pocket |
| 0x241F | 0x2420 | 42 | 0x2400 | 0x2402 | 42 | Item pocket item list |
| 0x2449 | 0x244A | 27 | 0x242A | 0x242C | 27 | Key item pocket item list |
| 0x2464 | 0x2465 | 26 | 0x2445 | 0x2447 | 26 | Ball pocket item list |
| 0x247E | 0x247F | 102 | 0x245F | 0x2461 | 102 | PC item list |
| 0x2724 | 0x2700 | 3 | 0x2705 | 0x26E2 | 3 | Current PC Box number |
| 0x2727 | 0x2703 | 127 | 0x2708 | 0x26E5 | 81 | PC Box names |
| 0x2851 | -- | 2 | -- | -- | -- | Lucky Number Show weekly number |
| -- | 0x2780 | 28 | -- | 0x2735 | 28 | Saved map header |
| 0x2868 | 0x2843 | 4 | 0x281C | 0x27F8 | 4 | Player location |
| 0x286C | 0x2847 | 30 | 0x2820 | 0x27FC | 30 | Saved map tiles |
| 0x288A | 0x2865 | 428 | 0x283E | 0x281A | 368 | Party Pokemon list |
| 0x2A4C | 0x2A27 | 32 | 0x29CE | 0x29AA | 32 | Pokedex owned |
| 0x2A6C | 0x2A47 | 32 | 0x29EE | 0x29CA | 32 | Pokedex seen |
| 0x2AA9 | -- | 11 | -- | -- | -- | Pokemon Day Care Pokemon #1 Name |
| 0x2ABF | -- | 32 | -- | -- | -- | Pokemon Day Care Pokemon #1 Data |
| 0x2AC8 | -- | 2 | -- | -- | -- | Pokemon Day Care step count |
| 0x2AE0 | -- | 2 | -- | -- | -- | Pokemon Day Care incr/decr per step |
| 0x2B01 | -- | 2 | -- | -- | -- | Pokemon Day Care step count |
| 0x2AE2 | -- | 11 | -- | -- | -- | Pokemon Day Care Pokemon #2 Name |
| 0x2AF8 | -- | 32 | -- | -- | -- | Pokemon Day Care Pokemon #2 Data |
| 0x2D6C | 0x2D10 | 1102 | 0x2D10 | 0x2D10 | 1352 | Current Box Pokemon list |
| N/A | 0x3E3D | 1 | N/A | 0x8000 | 1 | Player gender |
| 0x4000 | 0x4000 | 1102 | 0x4000 | 0x4000 | 1352 | PC Box 1 Pokemon list |
| 0x4450 | 0x4450 | 1102 | 0x454A | 0x454A | 1352 | PC Box 2 Pokemon list |
| 0x48A0 | 0x48A0 | 1102 | 0x4A94 | 0x4A94 | 1352 | PC Box 3 Pokemon list |
| 0x4CF0 | 0x4CF0 | 1102 | 0x4FDE | 0x4FDE | 1352 | PC Box 4 Pokemon list |
| 0x5140 | 0x5140 | 1102 | 0x5528 | 0x5528 | 1352 | PC Box 5 Pokemon list |
| 0x5590 | 0x5590 | 1102 | 0x5A72 | 0x5A72 | 1352 | PC Box 6 Pokemon list |
| 0x59E0 | 0x59E0 | 1102 | 0x6000 | 0x6000 | 1352 | PC Box 7 Pokemon list |
| 0x6000 | 0x6000 | 1102 | 0x654A | 0x654A | 1352 | PC Box 8 Pokemon list |
| 0x6450 | 0x6450 | 1102 | 0x6A94 | 0x6A94 | 1352 | PC Box 9 Pokemon list |
| 0x68A0 | 0x68A0 | 1102 | N/A | N/A | -- | PC Box 10 Pokemon list |
| 0x6CF0 | 0x6CF0 | 1102 | N/A | N/A | -- | PC Box 11 Pokemon list |
| 0x7140 | 0x7140 | 1102 | N/A | N/A | -- | PC Box 12 Pokemon list |
| 0x7590 | 0x7590 | 1102 | N/A | N/A | -- | PC Box 13 Pokemon list |
| 0x79E0 | 0x79E0 | 1102 | N/A | N/A | -- | PC Box 14 Pokemon list |
| 0x2D69 | 0x2D0D | 2 | -- | 0x2D0D | 2 | Checksum 1 |
| 0x7E6D | 0x1F0D | 2 | -- | 0x7F0D | 2 | Checksum 2 |

### Player Trainer ID

Represents the Trainer ID number in two bytes, which can range from 0 to 65535. In Crystal, the boy player character will always have an even number ID, while the girl's ID will always be odd.

### Player name

Represents text strings that can be from 1 to 7 characters in length, although the save structure allocates 11 bytes for the name. In the Japanese version, the amount of characters is reduced from 7 to 5, and the save structure allocates 6 bytes instead of 11.

The first 8 bytes contain the name with any leftover equal to 0x50. Since the name can be 7 bytes at most, the eighth byte (or sixth byte in the Japanese version) will always be 0x50. The remaining 3 bytes are all 0x00.

### Rival name

Same structure as Player name. The remaining 9th, 10th, and 11th bytes are equal to 0x86, 0x91, 0x84 respectively.

### Daylight savings

The highest bit of this field is set to indicate DST is in effect. The lower 7 bits of this field have unknown significance.

### Time played

4 one-byte values representing, in this order: the hours, minutes, seconds and "frames" that have elapsed. A frame is 1/60th of a second.

This timer is not halted when the game is paused, and also counts up on the main menu before selecting to continue a saved game.

### Player palette

The lowest 3 bits of this field are transferred to OAM to select the colors when drawing the player character. 8 possible palettes:

| Value | Color |
|-------|-------|
| 0x00 | Red |
| 0x01 | Blue |
| 0x02 | Green |
| 0x03 | Brown |
| 0x04 | Orange |
| 0x05 | Gray |
| 0x06 | Dark Green |
| 0x07 | Dark Red |

For boy characters, this is set to 0x00 (red). For girl characters, this is set to 0x01 (blue). Despite only being able to make boy characters in Gold and Silver, this field is still present and functional.

### Johto Badges

The eight badges are stored on eight bits, one bit for each badge; '1' means the badge is acquired, '0' otherwise. From MSB to LSB: Zephyr, Insect, Plain, Fog, Storm, Mineral, Glacier, Rising.

### TM pocket

| Offset | Size | Contents |
|--------|------|----------|
| 0x00 | 50 | TMs list |
| 0x32 | 7 | HMs list |

**TMs list:** Each byte specifies the quantity of the corresponding TM that the player is holding. Should be 0 to 99. Indexes match item numbers: 0 = TM01, 49 = TM50.

**HMs list:** Each byte specifies the quantity of the corresponding HM that the player is holding. Should be 0 to 1. Indexes match item numbers: 0 = HM01, 6 = HM07.

### Item pocket item list

Stored as an Item list with a capacity of 20.

### Key item pocket item list

Stored as an Item list with a capacity of 26.

### Ball pocket item list

Stored as an Item list with a capacity of 12.

### PC item list

Stored as an Item list with a capacity of 50.

### Current PC Box

Indicates which PC box is currently selected, minus 1. Box 1 is represented as 0, and box 14 is represented as 13. The lowest 4 bits of this value are the box index.

### PC Box names

The 9 (Japanese game) or 14 (English game) box names. Each name is a string between 1 and 8 characters plus a terminator byte, for a total of 9 bytes each.

### Lucky Number Show weekly number

This week's number for the Lucky Number Show. This field is populated the first time each week when the player listens to the Lucky Number Show or visits the Radio Tower to redeem their prize. If that has not yet occurred, this value will contain a previous week's lucky number.

### Player location

The map bank and map number for the map the player is currently in, followed by the player's X/Y position. Defaults to 0x18 0x07 0x03 0x03 upon starting a new game.

### Party Pokemon list

Stored as a Pokemon list with a capacity of 6 and an entry size of 48 bytes.

### Pokedex owned, Pokedex seen

The Pokemon registered in the Pokedex as seen or owned are represented by a pair of 32-byte little-endian bit arrays (for a total of 256 bits). Each bit represents whether a particular Pokemon has been seen/owned or not.

Pokemon are listed in National Pokedex order, with bit 0 corresponding to #001 Bulbasaur, bit 1 corresponding to #002 Ivysaur, and so on, up to bit 250 corresponding to #251 Celebi. Bits 251-255 are unused.

### PC Box Pokemon lists

Stored as Pokemon lists with a capacity of 20 Pokemon in English save files and 30 in Japanese ones, and an entry size of 32 bytes. After every list is the two bytes FF 00.

Normally, Pokemon are deposited and withdrawn from the Current Box list, which is within the checksum-validated region of the save data. When switching boxes, the data from the Current Box is copied to the corresponding PC Box data, then the data from the switched-to PC Box is transferred into the Current Box data.

### Player gender

0x00 for boy, 0x01 for girl. This field is not within the checksum-validated region.

### Checksums

Player data in Generation II is stored in the save file twice. The primary copy is located at 0x2009, and a secondary copy is stored elsewhere in the file. Checksums are performed on both copies and stored in the data.

- If only one checksum is correct, then the information from that copy of the data will be used.
- If both values are incorrect, the player will be forced to start a new game.

The checksums are simply the 16-bit sum of all byte values of the corresponding byte regions. Checksums are stored as little-endian.

#### Gold and Silver

The secondary data copy in Gold and Silver is not contiguous like the primary copy is. Instead, it is split across 5 sections and stored at different locations in the save file:

| Primary From | Primary To | Secondary From | Secondary To |
|--------------|------------|----------------|--------------|
| 0x2009 | 0x222E | 0x15C7 | 0x17EC |
| 0x222F | 0x23D8 | 0x3D96 | 0x3F3F |
| 0x23D9 | 0x2855 | 0x0C6B | 0x10E7 |
| 0x2856 | 0x2889 | 0x7E39 | 0x7E6C |
| 0x288A | 0x2D68 | 0x10E8 | 0x15C6 |

Calculating the checksums:
- Sum the bytes from 0x2009 to 0x2D68 and store the result at 0x2D69
- Sum the bytes from 0x0C6B to 0x17EC, 0x3D96 to 0x3F3F and 0x7E39 to 0x7E6C, and store the result at 0x7E6D

For the Japanese games, the checksum ranges are contiguous. The primary range goes from 0x2009 to 0x2C8B and is stored at 0x2D0D. The secondary range goes from 0x7209 to 0x7E8B and is stored at 0x7F0D.

#### Crystal

The secondary data copy in Crystal is a byte-for-byte match of the primary copy:

| Primary From | Primary To | Secondary From | Secondary To |
|--------------|------------|----------------|--------------|
| 0x2009 | 0x2B82 | 0x1209 | 0x1D82 |

Calculating the checksums:
- Sum the bytes from 0x2009 to 0x2B82 and store the result at 0x2D0D
- Sum the bytes from 0x1209 to 0x1D82 and store the result at 0x1F0D

For Japanese games, the secondary partition is from 0x7209 to 0x7D3A. Both checksums are calculated using 0xADA (2778) bytes from the initial location as opposed to the 2937 bytes in the English version:
- Sum the bytes from 0x2009 to 0x2AE2 and store the result at 0x2D0D
- Sum the bytes from 0x7209 to 0x7CE2 and store the result at 0x7F0D
