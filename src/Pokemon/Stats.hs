-- | Experience curves and stat calculation for Gen 1/2.
--
-- Pure functions, no IO. The formulas are identical in both gens —
-- the only difference is that Gen 2 splits the Special base stat
-- into SpAtk/SpDef (modeled by Special). The DV and stat exp
-- for Special are shared between both in Gen 2.

module Pokemon.Stats
  ( -- * Experience curves
    expForLevel

    -- * Stat calculation (building blocks)
  , calcHP
  , calcStat
  , statExpBonus
  , isqrt

    -- * All stats at once
  , CalcStats (..)
  , calcAllStats
  ) where

import Pokemon.Types


-- ── Experience curves ────────────────────────────────────────

-- | Total experience needed to reach a given level.
-- Level 1 is always 0. MediumSlow goes negative at low levels;
-- the games clamp to 0.
expForLevel :: GrowthRate -> Int -> Int
expForLevel _          1 = 0
expForLevel MediumFast n = n * n * n
expForLevel MediumSlow n = max 0 $
  6 * n * n * n `div` 5 - 15 * n * n + 100 * n - 140
expForLevel Fast       n = 4 * n * n * n `div` 5
expForLevel Slow       n = 5 * n * n * n `div` 4


-- ── Stat calculation ─────────────────────────────────────────

-- | Calculate a non-HP stat.
--
-- @calcStat base dv statExp level@
--
-- >>> calcStat 65 15 65535 100
-- 238
calcStat :: Int -> Int -> Int -> Int -> Int
calcStat base dv statexp level =
  ((base + dv) * 2 + statExpBonus statexp) * level `div` 100 + 5

-- | Calculate HP. Same formula but +level+10 instead of +5.
--
-- Pass the derived HP DV (from 'dvHP'), not a raw DV field.
--
-- >>> calcHP 45 15 65535 100
-- 198
calcHP :: Int -> Int -> Int -> Int -> Int
calcHP base dv statexp level =
  ((base + dv) * 2 + statExpBonus statexp) * level `div` 100 + level + 10

-- | Stat exp contribution: floor(sqrt(statExp)) / 4.
-- Ranges from 0 (at 0 stat exp) to 63 (at 65535).
statExpBonus :: Int -> Int
statExpBonus statexp = isqrt statexp `div` 4

-- | Integer square root. Accurate for all 16-bit stat exp values.
isqrt :: Int -> Int
isqrt n = floor (sqrt (fromIntegral n) :: Double)


-- ── All stats at once ────────────────────────────────────────

-- | Calculated stats for a Pokemon. The Special field mirrors
-- Special: Unified in Gen 1, Split in Gen 2.
data CalcStats = CalcStats
  { statHP      :: !Int
  , statAttack  :: !Int
  , statDefense :: !Int
  , statSpeed   :: !Int
  , statSpecial :: !Special
  } deriving (Eq, Show)

-- | Calculate all stats from species data, DVs, stat exp, and level.
-- Pattern matches on Special to decide Gen 1 vs Gen 2 handling.
calcAllStats :: Species -> DVs -> StatExp -> Int -> CalcStats
calcAllStats species dvs statexp level = CalcStats
  { statHP      = calcHP   (baseHP bs)      (dvHP dvs)      (expHP statexp)      level
  , statAttack  = calcStat (baseAttack bs)  (dvAttack dvs)  (expAttack statexp)  level
  , statDefense = calcStat (baseDefense bs) (dvDefense dvs) (expDefense statexp) level
  , statSpeed   = calcStat (baseSpeed bs)   (dvSpeed dvs)   (expSpeed statexp)   level
  , statSpecial = case baseSpecial bs of
      Unified s   -> Unified (calcStat s  (dvSpecial dvs) (expSpecial statexp) level)
      Split sa sd -> Split   (calcStat sa (dvSpecial dvs) (expSpecial statexp) level)
                              (calcStat sd (dvSpecial dvs) (expSpecial statexp) level)
  }
  where
    bs = speciesBaseStats species
