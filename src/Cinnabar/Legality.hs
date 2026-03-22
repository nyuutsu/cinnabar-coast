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

module Cinnabar.Legality
  ( classifyMove
  , classifyMoveNoTradeback
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Cinnabar.Types


-- | Classify how a species can learn a move in a given generation.
--
-- Returns all matching LearnSources — empty list means not legal.
-- Pass the other gen's GameData for Tradeback checking, or Nothing
-- to skip it.
classifyMove
  :: GameData          -- ^ This gen's data
  -> Maybe GameData    -- ^ Other gen's data (for Tradeback)
  -> DexNumber         -- ^ Dex number
  -> MoveId            -- ^ Move ID
  -> Level             -- ^ Pokemon's level (for level-up cutoff)
  -> [LearnSource]
classifyMove thisGen otherGen dex moveId level =
  let learnsets = gameLearnsetData thisGen
      machines  = gameMachineData thisGen
      graph     = gameSpeciesGraph thisGen
  in concat
    [ checkLevelUp   (gameLevelUp learnsets) dex moveId level
    , checkMachine   machines dex moveId
    , checkEggMove   (gameEggMoves learnsets) dex moveId
    , checkTutorMove (gameTutorMoves learnsets) dex moveId
    , checkTradeback otherGen dex moveId level
    , checkPreEvo    graph learnsets machines otherGen dex moveId level
    -- EventMove: stub (needs event loader)
    ]


-- | Classify without Tradeback or PreEvo. Used internally to
-- prevent infinite recursion: Tradeback checks the other gen,
-- PreEvo checks pre-evolutions. Neither should recurse back.
classifyMoveNoTradeback :: GameData -> DexNumber -> MoveId -> Level -> [LearnSource]
classifyMoveNoTradeback gameData dex moveId level =
  let learnsets = gameLearnsetData gameData
      machines  = gameMachineData gameData
  in concat
    [ checkLevelUp   (gameLevelUp learnsets) dex moveId level
    , checkMachine   machines dex moveId
    , checkEggMove   (gameEggMoves learnsets) dex moveId
    , checkTutorMove (gameTutorMoves learnsets) dex moveId
    ]


-- ── Individual checks ──────────────────────────────────────────

-- | Check if the species learns this move by leveling up at or
-- below the given level.
checkLevelUp :: Map DexNumber [LevelUpEntry] -> DexNumber -> MoveId -> Level -> [LearnSource]
checkLevelUp levelUpMap dex moveId level =
  case Map.lookup dex levelUpMap of
    Nothing      -> []
    Just entries ->
      [ LearnSource LevelUp (Text.pack $ "L" ++ show (unLevel entryLevel)) []
      | LevelUpEntry entryLevel entryMoveId <- entries
      , entryMoveId == moveId
      , entryLevel <= level
      ]


-- | Check if there's a TM or HM that teaches this move and the
-- species is compatible with it.
checkMachine :: MachineData -> DexNumber -> MoveId -> [LearnSource]
checkMachine machineData dex moveId =
  case Map.lookup moveId (gameMoveToMachine machineData) of
    Nothing      -> []
    Just machine ->
      case Map.lookup dex (gameMachineCompat machineData) of
        Nothing               -> []
        Just compatibleMachines
          | Set.member machine compatibleMachines ->
              let (method, label) = case machine of
                    TM (MachineNumber machineNumber) -> (TMMachine, "TM" ++ padMachineNumber machineNumber)
                    HM (MachineNumber machineNumber) -> (HMMachine, "HM" ++ padMachineNumber machineNumber)
              in [LearnSource method (Text.pack label) []]
          | otherwise -> []


-- | Check if this is an egg move for the species (Gen 2 only).
checkEggMove :: Map DexNumber (Set MoveId) -> DexNumber -> MoveId -> [LearnSource]
checkEggMove eggMoveMap dex moveId =
  case Map.lookup dex eggMoveMap of
    Nothing   -> []
    Just moves
      | Set.member moveId moves -> [LearnSource EggMove "Egg" []]
      | otherwise               -> []


-- | Check if this is a tutor move for the species (Crystal only).
checkTutorMove :: Map DexNumber (Set MoveId) -> DexNumber -> MoveId -> [LearnSource]
checkTutorMove tutorMoveMap dex moveId =
  case Map.lookup dex tutorMoveMap of
    Nothing   -> []
    Just moves
      | Set.member moveId moves -> [LearnSource TutorMove "Tutor" []]
      | otherwise               -> []


-- | Check Tradeback: can this species learn the move in the
-- other generation? If so, return a Tradeback source that nests
-- the other gen's sources inside it.
checkTradeback :: Maybe GameData -> DexNumber -> MoveId -> Level -> [LearnSource]
checkTradeback Nothing _ _ _ = []
checkTradeback (Just otherGameData) dex moveId level =
  case classifyMoveNoTradeback otherGameData dex moveId level of
    []      -> []
    sources ->
      let genLabel = case gameGen otherGameData of
            Gen1 -> "Gen 1"
            Gen2 -> "Gen 2"
      in [LearnSource Tradeback genLabel sources]


-- | Check PreEvo: can any pre-evolution of this species learn
-- the move? Walks backward through the evolution chain and
-- checks each ancestor. Results nest the pre-evo's own sources.
--
-- Only reports moves the pre-evo can learn that the current
-- species CANNOT learn directly (otherwise it's redundant).
checkPreEvo
  :: SpeciesGraph -> LearnsetData -> MachineData -> Maybe GameData
  -> DexNumber -> MoveId -> Level -> [LearnSource]
checkPreEvo graph learnsets machines otherGen dex moveId level =
  let -- Collect direct sources for this species (what it can learn on its own,
      -- including tradeback but not pre-evo)
      directMethods = Set.fromList $ map sourceMethod $
        classifyMoveNoPreEvo learnsets machines otherGen dex moveId level

      -- Find all pre-evolutions by walking gameEvolvesFrom
      preEvos = allPreEvolutions graph dex

      -- For each pre-evo, check if it can learn the move
      -- (using full classification including tradeback, but not PreEvo
      -- to avoid infinite recursion)
      preEvoSources =
        [ LearnSource PreEvo (speciesLabel graph preEvoDex) sources
        | preEvoDex <- preEvos
        , let sources = classifyMoveNoPreEvo learnsets machines otherGen preEvoDex moveId level
        , not (null sources)
        -- Only include if the pre-evo has methods the current species doesn't
        , let preEvoMethods = Set.fromList $ map sourceMethod sources
        , not (Set.null (Set.difference preEvoMethods directMethods))
        ]
  in preEvoSources


-- | Classify a move for a species, including Tradeback but NOT
-- PreEvo. Used by checkPreEvo to avoid infinite recursion.
classifyMoveNoPreEvo
  :: LearnsetData -> MachineData -> Maybe GameData
  -> DexNumber -> MoveId -> Level -> [LearnSource]
classifyMoveNoPreEvo learnsets machines otherGen dex moveId level =
  concat
    [ checkLevelUp   (gameLevelUp learnsets) dex moveId level
    , checkMachine   machines dex moveId
    , checkEggMove   (gameEggMoves learnsets) dex moveId
    , checkTutorMove (gameTutorMoves learnsets) dex moveId
    , checkTradeback otherGen dex moveId level
    ]


-- | Walk backward through evolution chains to find ALL
-- pre-evolutions of a species (not just the immediate one).
-- Pichu → Pikachu → Raichu: allPreEvolutions for Raichu = [Pikachu, Pichu]
allPreEvolutions :: SpeciesGraph -> DexNumber -> [DexNumber]
allPreEvolutions graph dex = collectAncestors (Set.singleton dex) dex []
  where
    collectAncestors visited current ancestors =
      case Map.lookup current (gameEvolvesFrom graph) of
        Nothing    -> ancestors
        Just steps ->
          let parents = map stepFrom steps
          in foldl (visitParent visited) ancestors parents

    visitParent visited ancestorsSoFar parent
      -- Load-time cycle check in Data.hs prevents this from firing;
      -- kept as an assertion for direct callers outside the normal load path.
      | Set.member parent visited =
          error $ "allPreEvolutions: cycle detected at dex #" ++ show (unDex parent)
      | otherwise =
          collectAncestors (Set.insert parent visited) parent (parent : ancestorsSoFar)


-- ── Helpers ────────────────────────────────────────────────────

-- | Zero-pad a machine number to 2 digits: 1 → "01", 24 → "24".
padMachineNumber :: Int -> String
padMachineNumber machineNumber
  | machineNumber < 10    = "0" ++ show machineNumber
  | otherwise = show machineNumber

-- | Look up a species name for display, falling back to dex number.
speciesLabel :: SpeciesGraph -> DexNumber -> Text
speciesLabel graph dex =
  case Map.lookup dex (gameSpecies graph) of
    Just species -> speciesName species <> " (#" <> Text.pack (show (unDex dex)) <> ")"
    Nothing      -> "#" <> Text.pack (show (unDex dex))
