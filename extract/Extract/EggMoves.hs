{-# LANGUAGE OverloadedStrings #-}

-- | Extract egg move data from pokecrystal's egg_moves.asm.
--
-- Gen 2 only (Gen 1 has no breeding). The data lives in two files:
--
--   egg_move_pointers.asm — dw table mapping dex number → label
--   egg_moves.asm         — labeled blocks of db MOVE_NAME, db -1
--
-- We parse both and join on label name to produce (dex, move_name) pairs.

module Extract.EggMoves (extractEggMoves, eggMovesHeader) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Extract.ASM


-- | Extract egg moves from pokecrystal.
-- Takes paths to the pointer table and the egg moves file.
-- Returns CSV rows: [dex, move_name]
extractEggMoves :: FilePath -> FilePath -> IO [[Text]]
extractEggMoves pointersPath eggMovesPath = do
  pointers <- parseFile parsePointerTable pointersPath
  blocks   <- parseFile parseEggMoveBlocks eggMovesPath
  let blockMap = Map.fromList blocks
  pure [ [T.pack (show dex), move]
       | (dex, blockName) <- zip [1 :: Int ..] pointers
       , blockName /= "NoEggMoves"
       , move <- Map.findWithDefault [] blockName blockMap
       ]

-- | Parse the pointer table: a sequence of `dw LabelName` lines.
-- Returns label names in order (position = dex number).
parsePointerTable :: Parser [Text]
parsePointerTable = collectPointers []
  where
    collectPointers pointers = do
      done <- option False (True <$ eof)
      if done
        then pure (reverse pointers)
        else do
          horizontalSpace
          choice
            [ try (parseDw >>= \name -> collectPointers (name : pointers))
            , restOfLine >> collectPointers pointers
            ]

    parseDw = keyword "dw" *> identifier <* restOfLine
    restOfLine = takeWhileP Nothing (/= '\n') *> endOfLine

-- | Parse egg move blocks: labeled sections of db MOVE_NAME entries.
-- Returns (label, [move_name]) pairs.
parseEggMoveBlocks :: Parser [(Text, [Text])]
parseEggMoveBlocks = collectBlocks []
  where
    collectBlocks blocks = do
      done <- option False (True <$ eof)
      if done
        then pure (reverse blocks)
        else do
          horizontalSpace
          choice
            [ try $ do
                name <- parseLabel
                moves <- many (try parseDbMove)
                collectBlocks ((name, moves) : blocks)
            , restOfLine >> collectBlocks blocks
            ]

    -- A label: identifier followed by colon at start of line.
    parseLabel = do
      name <- takeWhile1P (Just "label") (\char -> char /= ':' && char /= '\n' && char /= ' ' && char /= '\t')
      _ <- single ':'
      restOfLine
      pure name

    -- db MOVE_NAME — the identifier parser rejects `db -1` naturally
    -- (because -1 starts with '-', not a letter), so the terminator
    -- line falls through to skipLine.
    parseDbMove = do
      horizontalSpace
      _ <- keyword "db"
      move <- identifier
      restOfLine
      pure move

    restOfLine = takeWhileP Nothing (/= '\n') *> endOfLine

eggMovesHeader :: [Text]
eggMovesHeader = ["dex", "move_name"]
