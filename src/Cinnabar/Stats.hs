-- | Experience curves and stat calculation for Gen 1/2.
--
-- Pure functions, no IO. The formulas are identical in both gens —
-- the only difference is that Gen 2 splits the Special base stat
-- into SpAtk/SpDef (modeled by Special). The DV and stat exp
-- for Special are shared between both in Gen 2.

module Cinnabar.Stats
  ( -- * Experience curves
    expForLevel

    -- * Stat calculation (building blocks)
  , StatInput (..)
  , calcHP
  , calcStat
  , statExpBonus
  , isqrt

    -- * All stats at once
  , CalcStats (..)
  , calcAllStats
  ) where

import Cinnabar.Types


-- ── Experience curves ────────────────────────────────────────

-- | Total experience needed to reach a given level.
-- Level 1 is always 0. MediumSlow goes negative at low levels;
-- the games clamp to 0.
expForLevel :: GrowthRate -> Level -> Int
expForLevel _          (Level 1) = 0
expForLevel MediumFast (Level level) = level * level * level
expForLevel MediumSlow (Level level) = max 0 $
  (6 * level * level * level) `div` 5 - 15 * level * level + 100 * level - 140
expForLevel Fast       (Level level) = (4 * level * level * level) `div` 5
expForLevel Slow       (Level level) = (5 * level * level * level) `div` 4


-- ── Stat calculation ─────────────────────────────────────────

-- | Inputs for a single stat calculation: the base stat value,
-- the DV (0-15), and the stat experience (0-65535).
data StatInput = StatInput
  { statBase    :: !BaseStat
  , statDV      :: !DV
  , statStatExp :: !StatExpPoints
  } deriving (Eq, Show)

-- | Calculate a non-HP stat.
--
-- >>> calcStat (StatInput (BaseStat 65) (DV 15) (StatExpPoints 65535)) (Level 100)
-- StatValue 228
calcStat :: StatInput -> Level -> StatValue
calcStat StatInput{statBase, statDV, statStatExp} (Level level) =
  StatValue $ ((unBaseStat statBase + unDV statDV) * 2 + statExpBonus statStatExp) * level `div` 100 + 5

-- | Calculate HP. Same formula but +level+10 instead of +5.
--
-- Pass the derived HP DV (from 'dvHP'), not a raw DV field.
--
-- >>> calcHP (StatInput (BaseStat 45) (DV 15) (StatExpPoints 65535)) (Level 100)
-- StatValue 293
calcHP :: StatInput -> Level -> StatValue
calcHP StatInput{statBase, statDV, statStatExp} (Level level) =
  StatValue $ ((unBaseStat statBase + unDV statDV) * 2 + statExpBonus statStatExp) * level `div` 100 + level + 10

-- | Stat exp contribution: floor(sqrt(statExp)) / 4.
-- Ranges from 0 (at 0 stat exp) to 63 (at 65535).
statExpBonus :: StatExpPoints -> Int
statExpBonus (StatExpPoints points) = isqrt points `div` 4

-- | Integer square root. Accurate for all 16-bit stat exp values.
isqrt :: Int -> Int
isqrt value = floor (sqrt (fromIntegral value) :: Double)


-- ── All stats at once ────────────────────────────────────────

-- | Calculated stats for a Pokemon. The Special field mirrors
-- Special: Unified in Gen 1, Split in Gen 2.
data CalcStats = CalcStats
  { statHP      :: !StatValue
  , statAttack  :: !StatValue
  , statDefense :: !StatValue
  , statSpeed   :: !StatValue
  , statSpecial :: !(Special StatValue)
  } deriving (Eq, Show)

-- | Calculate all stats from species data, DVs, stat exp, and level.
-- Pattern matches on Special to decide Gen 1 vs Gen 2 handling.
calcAllStats :: Species -> DVs -> StatExp -> Level -> CalcStats
calcAllStats species dvs statExp level = CalcStats
  { statHP      = calcHP   StatInput { statBase = baseHP baseStats,      statDV = dvHP dvs,      statStatExp = expHP statExp }      level
  , statAttack  = calcStat StatInput { statBase = baseAttack baseStats,  statDV = dvAttack dvs,  statStatExp = expAttack statExp }  level
  , statDefense = calcStat StatInput { statBase = baseDefense baseStats, statDV = dvDefense dvs, statStatExp = expDefense statExp } level
  , statSpeed   = calcStat StatInput { statBase = baseSpeed baseStats,   statDV = dvSpeed dvs,   statStatExp = expSpeed statExp }   level
  , statSpecial = case baseSpecial baseStats of
      Unified spcBase       -> Unified (calcStat StatInput { statBase = spcBase,   statDV = specialDV, statStatExp = specialExp } level)
      Split spAtkBase spDefBase -> Split   (calcStat StatInput { statBase = spAtkBase, statDV = specialDV, statStatExp = specialExp } level)
                                           (calcStat StatInput { statBase = spDefBase, statDV = specialDV, statStatExp = specialExp } level)
  }
  where
    baseStats  = speciesBaseStats species
    specialDV  = dvSpecial dvs
    specialExp = expSpecial statExp
