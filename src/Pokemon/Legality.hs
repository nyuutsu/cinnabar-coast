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
-- PreEvo walks backward through evolution chains to find moves
-- learnable by earlier stages but not the current species.
--
-- EventMove is a stub until event data is loaded.

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
    , checkPreEvo    thisGen otherGen dex moveId level
    -- EventMove: stub (needs event loader)
    ]


-- | Classify without Tradeback or PreEvo. Used internally to
-- prevent infinite recursion: Tradeback checks the other gen,
-- PreEvo checks pre-evolutions. Neither should recurse back.
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


-- | Check PreEvo: can any pre-evolution of this species learn
-- the move? Walks backward through the evolution chain and
-- checks each ancestor. Results nest the pre-evo's own sources.
--
-- Only reports moves the pre-evo can learn that the current
-- species CANNOT learn directly (otherwise it's redundant).
checkPreEvo :: GameData -> Maybe GameData -> Int -> Int -> Int -> [LearnSource]
checkPreEvo thisGen otherGen dex moveId level =
  let -- Collect direct sources for this species (what it can learn on its own,
      -- including tradeback but not pre-evo)
      directMethods = Set.fromList $ map sourceMethod $
        classifyMoveNoPreEvo thisGen otherGen dex moveId level

      -- Find all pre-evolutions by walking gameEvolvesFrom
      preEvos = allPreEvolutions thisGen dex

      -- For each pre-evo, check if it can learn the move
      -- (using full classification including tradeback, but not PreEvo
      -- to avoid infinite recursion)
      preEvoSources =
        [ LearnSource PreEvo (speciesLabel thisGen preEvoDex) sources
        | preEvoDex <- preEvos
        , let sources = classifyMoveNoPreEvo thisGen otherGen preEvoDex moveId level
        , not (null sources)
        -- Only include if the pre-evo has methods the current species doesn't
        , let preEvoMethods = Set.fromList $ map sourceMethod sources
        , not (Set.null (Set.difference preEvoMethods directMethods))
        ]
  in preEvoSources


-- | Classify a move for a species, including Tradeback but NOT
-- PreEvo. Used by checkPreEvo to avoid infinite recursion.
classifyMoveNoPreEvo :: GameData -> Maybe GameData -> Int -> Int -> Int -> [LearnSource]
classifyMoveNoPreEvo thisGen otherGen dex moveId level =
  concat
    [ checkLevelUp   thisGen dex moveId level
    , checkMachine   thisGen dex moveId
    , checkEggMove   thisGen dex moveId
    , checkTutorMove thisGen dex moveId
    , checkTradeback otherGen dex moveId level
    ]


-- | Walk backward through evolution chains to find ALL
-- pre-evolutions of a species (not just the immediate one).
-- Pichu → Pikachu → Raichu: allPreEvolutions for Raichu = [Pikachu, Pichu]
allPreEvolutions :: GameData -> Int -> [Int]
allPreEvolutions gd dex = go dex []
  where
    go current acc =
      case Map.lookup current (gameEvolvesFrom gd) of
        Nothing    -> acc
        Just steps ->
          let parents = map stepFrom steps
          in foldl (\a p -> go p (p : a)) acc parents


-- ── Helpers ────────────────────────────────────────────────────

-- | Zero-pad a machine number to 2 digits: 1 → "01", 24 → "24".
padNum :: Int -> String
padNum n
  | n < 10    = "0" ++ show n
  | otherwise = show n

-- | Look up a species name for display, falling back to dex number.
speciesLabel :: GameData -> Int -> T.Text
speciesLabel gd dex =
  case Map.lookup dex (gameSpecies gd) of
    Just sp -> speciesName sp <> " (#" <> T.pack (show dex) <> ")"
    Nothing -> "#" <> T.pack (show dex)
