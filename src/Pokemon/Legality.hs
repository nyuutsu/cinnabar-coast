{-# LANGUAGE OverloadedStrings #-}

-- | Move classification: how can a species learn a given move?
--
-- Pure functions. Takes GameData (one gen's static data) and returns
-- all legal paths by which a species could know a move. An empty
-- result means the move has no known legal source for that species.
--
-- Tradeback is supported: pass the other gen's GameData to check
-- moves learnable by trading across generations.
--
-- PreEvo and EventMove are stubs until evolution chains and event
-- data are loaded.

module Pokemon.Legality
  ( classifyMove
  , classifyMoveNoTradeback
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Pokemon.Types


-- | Classify how a species can learn a move in a given generation.
--
-- Returns all matching LearnSources — empty list means not legal.
-- Pass the other gen's GameData for Tradeback checking, or Nothing
-- to skip it.
classifyMove
  :: GameData          -- ^ This gen's data
  -> Maybe GameData    -- ^ Other gen's data (for Tradeback)
  -> Int               -- ^ Dex number
  -> Int               -- ^ Move ID
  -> Int               -- ^ Pokemon's level (for level-up cutoff)
  -> [LearnSource]
classifyMove thisGen otherGen dex moveId level =
  concat
    [ checkLevelUp   thisGen dex moveId level
    , checkMachine   thisGen dex moveId
    , checkEggMove   thisGen dex moveId
    , checkTutorMove thisGen dex moveId
    , checkTradeback otherGen dex moveId level
    -- PreEvo: stub (needs evolution chain data)
    -- EventMove: stub (needs event loader)
    ]


-- | Classify without Tradeback. Used internally to prevent
-- infinite recursion (Tradeback checks the other gen, which
-- must not check back again).
classifyMoveNoTradeback :: GameData -> Int -> Int -> Int -> [LearnSource]
classifyMoveNoTradeback gd dex moveId level =
  concat
    [ checkLevelUp   gd dex moveId level
    , checkMachine   gd dex moveId
    , checkEggMove   gd dex moveId
    , checkTutorMove gd dex moveId
    ]


-- ── Individual checks ──────────────────────────────────────────

-- | Check if the species learns this move by leveling up at or
-- below the given level.
checkLevelUp :: GameData -> Int -> Int -> Int -> [LearnSource]
checkLevelUp gd dex moveId level =
  case Map.lookup dex (gameLevelUp gd) of
    Nothing      -> []
    Just entries ->
      [ LearnSource LevelUp (T.pack $ "L" ++ show lvl) []
      | (lvl, mid) <- entries
      , mid == moveId
      , lvl <= level
      ]


-- | Check if there's a TM or HM that teaches this move and the
-- species is compatible with it.
checkMachine :: GameData -> Int -> Int -> [LearnSource]
checkMachine gd dex moveId =
  case Map.lookup dex (gameMachineCompat gd) of
    Nothing      -> []
    Just compat  ->
      [ LearnSource method (T.pack label) []
      | (machine, mid) <- Map.toList (gameMachines gd)
      , mid == moveId
      , Set.member machine compat
      , let (method, label) = case machine of
              TM n -> (TMMachine, "TM" ++ padNum n)
              HM n -> (HMMachine, "HM" ++ padNum n)
      ]


-- | Check if this is an egg move for the species (Gen 2 only).
checkEggMove :: GameData -> Int -> Int -> [LearnSource]
checkEggMove gd dex moveId =
  case Map.lookup dex (gameEggMoves gd) of
    Nothing   -> []
    Just moves
      | Set.member moveId moves -> [LearnSource EggMove "Egg" []]
      | otherwise               -> []


-- | Check if this is a tutor move for the species (Crystal only).
checkTutorMove :: GameData -> Int -> Int -> [LearnSource]
checkTutorMove gd dex moveId =
  case Map.lookup dex (gameTutorMoves gd) of
    Nothing   -> []
    Just moves
      | Set.member moveId moves -> [LearnSource TutorMove "Tutor" []]
      | otherwise               -> []


-- | Check Tradeback: can this species learn the move in the
-- other generation? If so, return a Tradeback source that nests
-- the other gen's sources inside it.
checkTradeback :: Maybe GameData -> Int -> Int -> Int -> [LearnSource]
checkTradeback Nothing _ _ _ = []
checkTradeback (Just otherGd) dex moveId level =
  case classifyMoveNoTradeback otherGd dex moveId level of
    []      -> []
    sources ->
      let genLabel = case gameGen otherGd of
            Gen1 -> "Gen 1"
            Gen2 -> "Gen 2"
      in [LearnSource Tradeback genLabel sources]


-- ── Helpers ────────────────────────────────────────────────────

-- | Zero-pad a machine number to 2 digits: 1 → "01", 24 → "24".
padNum :: Int -> String
padNum n
  | n < 10    = "0" ++ show n
  | otherwise = show n
