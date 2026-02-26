#!/usr/bin/env python3
"""Extract evolution data from pret/pokered and pret/pokecrystal ASM sources.

Usage:
    python scripts/generate_evolutions.py /path/to/pokered /path/to/pokecrystal

Outputs: data/csv/evolutions.csv
    gen,from_dex,to_dex,method,param
"""

import re
import sys
from pathlib import Path


def parse_pokemon_constants(repo: Path) -> dict[str, int]:
    """Parse pokemon_constants.asm -> {NAME: dex_number}.

    In pokecrystal (and pokered's later format), constants are defined
    in National Dex order starting from 1.
    """
    text = (repo / "constants" / "pokemon_constants.asm").read_text()
    result: dict[str, int] = {}
    index = -1
    for line in text.splitlines():
        m = re.match(r"\s*const\s+(\w+)", line)
        if m:
            index += 1
            result[m.group(1)] = index
            continue
        m2 = re.match(r"\s*const_def\s*(\d+)?", line)
        if m2:
            if m2.group(1):
                index = int(m2.group(1)) - 1
            else:
                index = -1
    return result


def parse_item_constants(repo: Path) -> dict[str, int]:
    """Parse item_constants.asm -> {NAME: item_id}."""
    text = (repo / "constants" / "item_constants.asm").read_text()
    result: dict[str, int] = {}
    index = -1  # const_def with no arg starts at 0
    for line in text.splitlines():
        m = re.match(r"\s*const\s+(\w+)", line)
        if m:
            index += 1
            result[m.group(1)] = index
            continue
        # const_def sets the starting value (default 0)
        m2 = re.match(r"\s*const_def\s*(\d+)?", line)
        if m2:
            if m2.group(1):
                index = int(m2.group(1)) - 1
            else:
                index = -1
    return result


def parse_gen1_evolutions(
    pokered: Path,
    pokemon_dex: dict[str, int],
    item_ids: dict[str, int],
) -> list[tuple[int, int, str, str]]:
    """Parse pokered evos_moves.asm -> [(from_dex, to_dex, method, param), ...]."""
    text = (pokered / "data" / "pokemon" / "evos_moves.asm").read_text()

    # Map labels to constant names
    label_to_const: dict[str, str] = {}
    for name in pokemon_dex:
        parts = name.split("_")
        label = "".join(p.capitalize() for p in parts)
        label_to_const[label + "EvosMoves"] = name

    results: list[tuple[int, int, str, str]] = []
    current_species: str | None = None

    for line in text.splitlines():
        line = line.strip()

        # Check for label
        m_label = re.match(r"(\w+EvosMoves):", line)
        if m_label:
            current_species = label_to_const.get(m_label.group(1))
            continue

        if not current_species:
            continue

        # End of evolutions section
        if line.startswith("db 0"):
            if "no more evolutions" in line or "db 0" == line:
                current_species = None
            continue

        # EVOLVE_LEVEL, level, species
        m = re.match(r"db\s+EVOLVE_LEVEL,\s*(\d+),\s*(\w+)", line)
        if m and current_species:
            from_dex = pokemon_dex.get(current_species, 0)
            to_dex = pokemon_dex.get(m.group(2), 0)
            if from_dex and to_dex:
                results.append((from_dex, to_dex, "level", m.group(1)))
            continue

        # EVOLVE_ITEM, item, min_level, species
        m = re.match(r"db\s+EVOLVE_ITEM,\s*(\w+),\s*\d+,\s*(\w+)", line)
        if m and current_species:
            from_dex = pokemon_dex.get(current_species, 0)
            to_dex = pokemon_dex.get(m.group(2), 0)
            item_name = m.group(1)
            item_id = item_ids.get(item_name, 0)
            if from_dex and to_dex:
                results.append((from_dex, to_dex, "item", str(item_id)))
            continue

        # EVOLVE_TRADE, min_level, species
        m = re.match(r"db\s+EVOLVE_TRADE,\s*\d+,\s*(\w+)", line)
        if m and current_species:
            from_dex = pokemon_dex.get(current_species, 0)
            to_dex = pokemon_dex.get(m.group(1), 0)
            if from_dex and to_dex:
                results.append((from_dex, to_dex, "trade", ""))
            continue

    return results


def parse_gen2_evolutions(
    crystal: Path,
    pokemon_dex: dict[str, int],
    item_ids: dict[str, int],
) -> list[tuple[int, int, str, str]]:
    """Parse pokecrystal evos_attacks.asm -> [(from_dex, to_dex, method, param), ...]."""
    text = (crystal / "data" / "pokemon" / "evos_attacks.asm").read_text()

    label_to_const: dict[str, str] = {}
    for name in pokemon_dex:
        parts = name.split("_")
        label = "".join(p.capitalize() for p in parts)
        label_to_const[label + "EvosAttacks"] = name

    results: list[tuple[int, int, str, str]] = []
    current_species: str | None = None

    for line in text.splitlines():
        line = line.strip()

        m_label = re.match(r"(\w[\w-]*EvosAttacks):", line)
        if m_label:
            current_species = label_to_const.get(m_label.group(1))
            continue

        if not current_species:
            continue

        if "no more evolutions" in line or (line == "db 0" and current_species):
            current_species = None
            continue

        from_dex = pokemon_dex.get(current_species, 0)
        if not from_dex:
            continue

        # EVOLVE_LEVEL, level, species
        m = re.match(r"db\s+EVOLVE_LEVEL,\s*(\d+),\s*(\w+)", line)
        if m:
            to_dex = pokemon_dex.get(m.group(2), 0)
            if to_dex:
                results.append((from_dex, to_dex, "level", m.group(1)))
            continue

        # EVOLVE_ITEM, item, species
        m = re.match(r"db\s+EVOLVE_ITEM,\s*(\w+),\s*(\w+)", line)
        if m:
            to_dex = pokemon_dex.get(m.group(2), 0)
            item_name = m.group(1)
            item_id = item_ids.get(item_name, 0)
            if to_dex:
                results.append((from_dex, to_dex, "item", str(item_id)))
            continue

        # EVOLVE_TRADE, held_item_or_-1, species
        m = re.match(r"db\s+EVOLVE_TRADE,\s*(-?\d+|\w+),\s*(\w+)", line)
        if m:
            to_dex = pokemon_dex.get(m.group(2), 0)
            held = m.group(1)
            if to_dex:
                if held == "-1":
                    results.append((from_dex, to_dex, "trade", ""))
                else:
                    item_id = item_ids.get(held, 0)
                    results.append((from_dex, to_dex, "trade_item", str(item_id)))
            continue

        # EVOLVE_HAPPINESS, time_constant, species
        m = re.match(r"db\s+EVOLVE_HAPPINESS,\s*(\w+),\s*(\w+)", line)
        if m:
            to_dex = pokemon_dex.get(m.group(2), 0)
            time_const = m.group(1)
            if to_dex:
                if time_const == "TR_MORNDAY":
                    results.append((from_dex, to_dex, "happiness_day", ""))
                elif time_const == "TR_NITE":
                    results.append((from_dex, to_dex, "happiness_night", ""))
                else:
                    results.append((from_dex, to_dex, "happiness", ""))
            continue

        # EVOLVE_STAT, level, comparison, species
        m = re.match(r"db\s+EVOLVE_STAT,\s*(\d+),\s*(\w+),\s*(\w+)", line)
        if m:
            to_dex = pokemon_dex.get(m.group(3), 0)
            level = m.group(1)
            comparison = m.group(2)
            if to_dex:
                if comparison == "ATK_LT_DEF":
                    results.append((from_dex, to_dex, "stat_lt", level))
                elif comparison == "ATK_GT_DEF":
                    results.append((from_dex, to_dex, "stat_gt", level))
                elif comparison == "ATK_EQ_DEF":
                    results.append((from_dex, to_dex, "stat_eq", level))
            continue

    return results


def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} /path/to/pokered /path/to/pokecrystal")
        sys.exit(1)

    pokered = Path(sys.argv[1])
    crystal = Path(sys.argv[2])

    # pokecrystal has all 251 species in dex order
    pokemon_dex = parse_pokemon_constants(crystal)
    print(f"  {len(pokemon_dex)} pokemon constants")

    item_ids_g1 = parse_item_constants(pokered)
    print(f"  {len(item_ids_g1)} Gen 1 item constants")

    item_ids_g2 = parse_item_constants(crystal)
    print(f"  {len(item_ids_g2)} Gen 2 item constants")

    gen1 = parse_gen1_evolutions(pokered, pokemon_dex, item_ids_g1)
    print(f"  Gen 1: {len(gen1)} evolution steps")

    gen2 = parse_gen2_evolutions(crystal, pokemon_dex, item_ids_g2)
    print(f"  Gen 2: {len(gen2)} evolution steps")

    outpath = Path(__file__).parent.parent / "data" / "csv" / "evolutions.csv"
    with open(outpath, "w") as f:
        f.write("gen,from_dex,to_dex,method,param\n")
        for from_dex, to_dex, method, param in gen1:
            f.write(f"1,{from_dex},{to_dex},{method},{param}\n")
        for from_dex, to_dex, method, param in gen2:
            f.write(f"2,{from_dex},{to_dex},{method},{param}\n")

    print(f"  Wrote {outpath} ({len(gen1) + len(gen2)} rows)")


if __name__ == "__main__":
    main()
