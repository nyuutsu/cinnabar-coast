{-# LANGUAGE OverloadedStrings #-}

-- | Extract TM/HM/tutor → move mappings from item_constants.asm.
--
-- This is a thin wrapper around parseTMHMBlock — it just formats
-- the result as CSV rows. The actual parsing logic lives in ASM.hs.

module Extract.TMHM (extractTMHM, tmhmHeader) where

import Data.Text (Text)
import qualified Data.Text as Text

import Extract.ASM


-- | Extract TM/HM → move mappings from one gen's item_constants.asm.
-- Returns CSV rows: [gen, number, move_name, kind]
-- where kind is "tm", "hm", or "tutor".
extractTMHM :: Text -> FilePath -> IO [[Text]]
extractTMHM gen path = do
  tmhm <- parseFile parseTMHMBlock path
  let tmRows    = zipWith (formatRow gen "tm")    [1..] (tmMoves tmhm)
      hmRows    = zipWith (formatRow gen "hm")    [1..] (hmMoves tmhm)
      tutorRows = zipWith (formatRow gen "tutor") [1..] (tutorMoves tmhm)
  pure (tmRows ++ hmRows ++ tutorRows)

formatRow :: Text -> Text -> Int -> Text -> [Text]
formatRow gen kind machineNumber moveName =
  [gen, Text.pack (show machineNumber), moveName, kind]

tmhmHeader :: [Text]
tmhmHeader = ["gen", "number", "move_name", "kind"]
