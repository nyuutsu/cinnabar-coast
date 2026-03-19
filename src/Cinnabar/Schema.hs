{-# LANGUAGE OverloadedStrings #-}

-- | Canonical name-to-type mappings for pret ASM constants.
--
-- Shared source of truth for the loading layer. Each Map contains
-- every recognized constant name for a domain type, built from a
-- literal association list rather than pattern-match chains. This
-- means adding a constructor to a sum type won't be silently
-- swallowed by a catch-all — the missing mapping is visible in
-- the association list.

module Cinnabar.Schema
  ( typeNames
  , growthRateNames
  , genderRatioNames
  , eggGroupNames
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Cinnabar.Types


-- ── Type names ───────────────────────────────────────────────────

-- | pret ASM type constant name → PokemonType.
-- PSYCHIC_TYPE is a pret alias that avoids collision with the move name.
typeNames :: Map.Map Text PokemonType
typeNames = Map.fromList
  [ ("NORMAL",       Normal)
  , ("FIGHTING",     Fighting)
  , ("FLYING",       Flying)
  , ("POISON",       Poison)
  , ("GROUND",       Ground)
  , ("ROCK",         Rock)
  , ("BUG",          Bug)
  , ("GHOST",        Ghost)
  , ("STEEL",        Steel)
  , ("FIRE",         Fire)
  , ("WATER",        Water)
  , ("GRASS",        Grass)
  , ("ELECTRIC",     Electric)
  , ("PSYCHIC",      Psychic)
  , ("PSYCHIC_TYPE", Psychic)
  , ("ICE",          Ice)
  , ("DRAGON",       Dragon)
  , ("DARK",         Dark)
  ]


-- ── Growth rate names ────────────────────────────────────────────

-- | pret ASM growth rate constant name → GrowthRate.
growthRateNames :: Map.Map Text GrowthRate
growthRateNames = Map.fromList
  [ ("GROWTH_MEDIUM_FAST", MediumFast)
  , ("GROWTH_MEDIUM_SLOW", MediumSlow)
  , ("GROWTH_FAST",        Fast)
  , ("GROWTH_SLOW",        Slow)
  ]


-- ── Gender ratio names ──────────────────────────────────────────

-- | pret ASM gender ratio constant name → GenderRatio.
genderRatioNames :: Map.Map Text GenderRatio
genderRatioNames = Map.fromList
  [ ("GENDER_F0",      AllMale)
  , ("GENDER_F12_5",   Female12_5)
  , ("GENDER_F25",     Female25)
  , ("GENDER_F50",     Female50)
  , ("GENDER_F75",     Female75)
  , ("GENDER_F100",    AllFemale)
  , ("GENDER_UNKNOWN", Genderless)
  ]


-- ── Egg group names ─────────────────────────────────────────────

-- | pret ASM egg group constant name → EggGroup.
eggGroupNames :: Map.Map Text EggGroup
eggGroupNames = Map.fromList
  [ ("EGG_MONSTER",       EggMonster)
  , ("EGG_WATER_1",       EggWater1)
  , ("EGG_BUG",           EggBug)
  , ("EGG_FLYING",        EggFlying)
  , ("EGG_GROUND",        EggGround)
  , ("EGG_FAIRY",         EggFairy)
  , ("EGG_PLANT",         EggPlant)
  , ("EGG_HUMANSHAPE",    EggHumanShape)
  , ("EGG_WATER_3",       EggWater3)
  , ("EGG_MINERAL",       EggMineral)
  , ("EGG_INDETERMINATE", EggIndeterminate)
  , ("EGG_WATER_2",       EggWater2)
  , ("EGG_DITTO",         EggDitto)
  , ("EGG_DRAGON",        EggDragon)
  , ("EGG_NONE",          EggNone)
  ]
